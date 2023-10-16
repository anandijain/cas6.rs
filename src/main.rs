extern crate peg;

use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

peg::parser! {
    grammar expr_parser() for str {
        rule whitespace() = [' ' | '\t' | '\n' | '\r']*

        rule symbol() -> Expr
            = s:$(['a'..='z' | 'A'..='Z' | '?' | '$'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' ]* ) { Expr::Sym(s.into()) }

        rule list() -> Expr
            = "(" l:Expr() ** whitespace() ")" { Expr::List(l) }

        pub rule Expr() -> Expr
            = symbol() / list()
    }
}

fn parse(s: &str) -> Expr {
    expr_parser::Expr(s).unwrap()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Sym(String),
    List(Vec<Expr>),
}

impl Deref for Expr {
    type Target = Vec<Expr>;

    fn deref(&self) -> &Self::Target {
        match self {
            Expr::List(vec) => vec,
            e => panic!("Can only deref Expr::List. ex:{e:?}"),
        }
    }
}

impl DerefMut for Expr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Expr::List(vec) => vec,
            e => panic!("Can only deref Expr::List. ex:{e:?}"),
        }
    }
}

fn sym(s: &str) -> Expr {
    Expr::Sym(s.to_string())
}

fn list(strs: Vec<&str>) -> Expr {
    Expr::List(strs.iter().map(|s| sym(s)).collect::<Vec<_>>())
}

fn liste(es: Vec<Expr>) -> Expr {
    Expr::List(es)
}
// head (f x) -> f, head(f) -> Sym
fn head(e: Expr) -> Expr {
    match e {
        Expr::Sym(_) => sym("Sym"),
        Expr::List(es) => es[0].clone(),
    }
}

fn pos_map_rebuild(pos: Vec<usize>, pat: Expr, pos_map: &HashMap<Vec<usize>, Expr>) -> Expr {
    if let Some(replacement) = pos_map.get(&pos) {
        return replacement.clone();
    }

    match pat {
        Expr::Sym(_) => pat,
        Expr::List(es) => {
            let mut new_es = vec![];
            for (i, e) in es.iter().enumerate() {
                let mut new_pos = pos.clone();
                new_pos.push(i);
                let new_e = pos_map_rebuild(new_pos, e.clone(), pos_map);
                new_es.push(new_e);
            }
            Expr::List(new_es)
        }
    }
}

fn named_rebuild_all(expr: Expr, map: &HashMap<Expr, Expr>) -> Expr {
    // First, check if the entire expression exists in the map and replace it if it does
    if let Some(replacement) = map.get(&expr) {
        return replacement.clone();
    }

    // If the expression is not in the map, proceed with the recursion
    match expr {
        Expr::Sym(_) => expr,
        Expr::List(list) => {
            // Recursively rebuild all sub-expressions in the list
            let new_list: Vec<Expr> = list
                .into_iter()
                .map(|e| named_rebuild_all(e, map))
                .collect();
            Expr::List(new_list)
        }
    }
}

fn rebuild_all(
    pat: Expr,
    named_map: &HashMap<Expr, Expr>,
    pos_map: &HashMap<Vec<usize>, Expr>,
) -> Expr {
    let new_ex = named_rebuild_all(pat, named_map);
    pos_map_rebuild(vec![], new_ex, pos_map)
}

fn splice_sequences(expr: Expr) -> Expr {
    match expr {
        Expr::Sym(_) => expr,
        Expr::List(mut list) => {
            let mut i = 0;
            while i < list.len() {
                list[i] = splice_sequences(list[i].clone());
                i += 1;
            }

            let mut new_list = Vec::new();
            let mut i = 0;
            while i < list.len() {
                let item = list[i].clone();
                if let Expr::List(ref sublist) = item {
                    if let Some(Expr::Sym(head)) = sublist.first() {
                        if head == "sequence" {
                            new_list.extend_from_slice(&sublist[1..]);
                            i += 1;
                            continue;
                        }
                    }
                }
                new_list.push(item);
                i += 1;
            }
            Expr::List(new_list)
        }
    }
}

fn rebuild_and_splice(
    pat: Expr,
    named_map: &HashMap<Expr, Expr>,
    pos_map: &HashMap<Vec<usize>, Expr>,
) -> Expr {
    splice_sequences(rebuild_all(pat, named_map, pos_map))
}

// we assume that p has a blank object for a head
fn is_blank_match(e: Expr, p: Expr) -> bool {
    if let Expr::List(ps) = p {
        if ps.len() == 2 {
            let p_head = &ps[1];
            if p_head == &head(e) {
                true
            } else {
                false
            }
        } else {
            true
        }
    } else {
        panic!("is_blank_match needs a list for p")
    }
}

// remember pos always refers to the place in the pattern, not the ex
fn my_match(
    ex: &Expr,
    pat: Expr,
    pos: &Vec<usize>,
    pos_map: &mut HashMap<Vec<usize>, Expr>,
    named_map: &mut HashMap<Expr, Expr>,
) -> bool {
    // todo move
    let blank_syms = vec![sym("blank"), sym("blank_seq"), sym("blank_null_seq")];
    match (ex.clone(), pat.clone()) {
        (Expr::Sym(e), Expr::Sym(p)) => e == p,
        (Expr::Sym(e), Expr::List(ps)) => {
            if ps[0] == sym("pattern") {
                if let Some(v) = named_map.get(&pat) {
                    // remember it may be the case that we actually recurse here but i dont really think so
                    v == ex
                } else {
                    if is_blank_match(ex.clone(), ps[2].clone()) {
                        named_map.insert(pat, ex.clone());
                        true
                    } else {
                        false
                    }
                }
            } else if blank_syms.contains(&ps[0]) {
                // we can assume that there is no key at our current position (we've never been here before),
                // or if we have, we removed the key appropriately
                assert!(!pos_map.contains_key(&pos.clone()));
                pos_map.insert(pos.to_vec(), ex.clone());
                true
            } else {
                false
            }
        }
        (Expr::List(es), Expr::List(ps)) => {
            // so if we are not in the else branch, we are matching the entire list ex
            if ps[0] == sym("pattern") {
                if let Some(v) = named_map.get(&pat) {
                    // remember it may be the case that we actually recurse here but i dont really think so
                    v == ex
                } else {
                    if is_blank_match(ex.clone(), ps[2].clone()) {
                        named_map.insert(pat, ex.clone());
                        true
                    } else {
                        false
                    }
                }
            } else if blank_syms.contains(&ps[0]) {
                // we can assume that there is no key at our current position (we've never been here before),
                // or if we have, we removed the key appropriately
                assert!(!pos_map.contains_key(pos));
                pos_map.insert(pos.to_vec(), ex.clone());
                true
            } else {
                let p_head = &ps[0];
                let p_rest = &ps[1..];

                let e_head = &es[0];
                let e_rest = &es[1..];

                let mut new_pos = pos.clone();
                new_pos.push(0); // zero is head position

                // (f x) | (f _)
                if !my_match(e_head, p_head.clone(), &new_pos, pos_map, named_map) {
                    // pos_map.remove()
                    return false;
                }

                // we skip the head, remember blank_seq and blank_null_seq can only match
                // (f a b) | ((blank_seq) (blank)) -> false because we cant "take" the a along with us for blank_seq
                for (i, p_i) in ps.iter().enumerate().skip(1) {
                    let mut new_pos = pos.clone();
                    new_pos.push(i); // zero is head position

                    match p_i {
                        Expr::List(pi_ps) => {
                            if pi_ps[0] == sym("pattern") {
                                let pi_blank = &pi_ps[2];
                                let blank_t = &pi_ps[2][0];
                                // actually this can throw, because its optional arg
                                // let pi_blank_h = &pi_ps[2][1];
                                let p_name = &pi_ps[1];

                                if blank_t == &sym("blank_seq") {
                                    for j in 1..=es[1..].len() {

                                        let mut elts = vec![sym("sequence")];
                                        elts.append(&mut es[i..i + j].to_vec());
                                        let seq = liste(elts);

                                        println!("seq {p_name:?}: {seq:?}");
                                        if let Some(from_map) = named_map.get(p_i) {
                                        } else {
                                            named_map.insert(p_i.clone(), seq.clone());
                                            // println!("here i am ");
                                        } // rebuild all uses the map to rebuild the pattern up to equality with no matching

                                        let new_ex =
                                            rebuild_and_splice(pat.clone(), named_map, pos_map);

                                        let m = my_match(ex, new_ex, pos, pos_map, named_map);
                                        if !m {
                                            named_map.remove(p_i);
                                            // since we are in a named pattern, we dont need this 
                                            // pos_map.remove(&new_pos);
                                        } else {
                                            return true;
                                        }
                                    }
                                }
                            }
                        }
                        _ => {
                            if !my_match(&es[i], p_i.clone(), &new_pos, pos_map, named_map) {
                                return false;
                            }
                        },
                    }
                }

                // what this is saying is:
                // if we've reached the end of the pattern list, finding mappings for all elements,
                // then we should be able to recreate ex from whatever was added to the maps
                let new_ex = rebuild_and_splice(pat.clone(), named_map, pos_map);
                if new_ex == ex.clone() {
                    return true;
                } else {
                    return false;
                }
            }
        }
        _ => false,
    }
}

fn main() {
    println!("Hello, world!");

    let blank = list(vec!["blank"]);

    let lhs = list(vec!["f", "a", "b"]);
    let rhs = list(vec!["f", "blank"]);

    let test_cases = vec![
        // (sym("1"), sym("1"), true),      // goes to "1" == "1" Sym, Sym arm
        // (sym("1"), blank.clone(), true), // Sym Sym arm with blank
        // (sym("1"), Expr::List(vec![sym("1")]), false), // Sym List -> false
        // (Expr::List(vec![sym("1")]), sym("1"), false), // List Sym
        // // (1) | (blank)
        // (Expr::List(vec![sym("1")]), blank.clone(), true), // List, sym, with blank
        // (lhs.clone(), rhs.clone(), false),                 // List, sym, with blank
        // // (lhs.clone(), list(vec!["f", "blank", "blank"]), true), // List, sym, with blank
        // (
        //     lhs.clone(),
        //     liste(vec![sym("f"), blank.clone(), blank.clone()]),
        //     true,
        // ), // List, sym, with blank
        // (sym("f"), list(vec!["blank", "Sym"]), true),
        // (sym("f"), list(vec!["blank", "f"]), false),
        // (list(vec!["f", "x"]), list(vec!["blank", "f"]), true),
        // (list(vec!["f", "x"]), list(vec!["blank", "g"]), false),
        // (parse("(f (a b))"), parse("(f (blank))"), true),
        // (parse("(f (a b))"), parse("(f (blank a))"), true),
        // (parse("(f x)"), parse("((blank) (blank))"), true),
        // (parse("f"), parse("(pattern x (blank))"), true),
        // (parse("(f)"), parse("(pattern x (blank))"), true),
        // (parse("(f x)"), parse("((pattern x (blank)) (blank))"), true),
        (parse("(f a b c)"), parse("(f (pattern x (blank_seq)))"), true),
        (parse("(f a b c)"), parse("(f (pattern x (blank_seq)) (pattern y (blank_seq)))"), true),
        (parse("(f a a)"), parse("(f (pattern x (blank_seq)) (pattern x (blank_seq)))"), true),
    ];

    // list(vec!["f", "a", "b", "c"]), list(vec!["f", sym("blank_sequence")])
    for (i, (ex, pat, expected)) in test_cases.iter().enumerate() {
        println!("testing case {i}: {ex:?} | {pat:?} ");
        let pos = vec![];
        let mut pos_map = HashMap::new();
        let mut named_map = HashMap::new();

        assert_eq!(
            my_match(ex, pat.clone(), &pos, &mut pos_map, &mut named_map),
            *expected
        );
        if *expected {
            let rebuilt_ex = rebuild_and_splice(pat.clone(), &named_map, &pos_map);
            assert_eq!(rebuilt_ex, ex.clone());
        }
        println!("pos:\n{pos_map:?}\nnamed:\n{named_map:?}\n\n");
    }
}
