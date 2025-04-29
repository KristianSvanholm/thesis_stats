use std::cmp::Ordering;
use std::fs;
use std::collections::HashMap;

fn main() {

    let chips = ["i78700", "n150", "epyc1", "epyc2", "m1max", "m2pro", "m3max"];

    let mut collect = HashMap::new();
    for chip in chips {
        let ranks: Vec<String> = fs::read_to_string(format!("data/{}/ranks.csv",chip))
            .expect("Should be able to read this file")
            .split("\n")
            .skip(1) // Skip title
            .filter(|p| !p.is_empty())
            .map(|entry| 
                entry.split(",")
                    .collect::<Vec<_>>()[1]
                    .to_string()
            )
            .collect();

        collect.insert(chip, ranks);
    }

    let keys: Vec<&str>= collect.keys().cloned().collect();

    for n in 0..keys.len() {
        for m in n+1..keys.len() {
            let a = keys[n];
            let b = keys[m];
            let tau = kendall(collect[a].clone(), collect[b].clone());
            println!("{:9} - {:9} | Tau: {:.3}", a,b,tau);
        }
    }

}

fn kendall<T: PartialEq>(a: Vec<T>, b: Vec<T> ) -> f32 {
    let mut ix = vec![];
    for elem in a {
        let i = b.iter().position(|x| *x == elem).unwrap();
        ix.push(i);
    }


    let mut con:f32 = 0.0;
    let mut dis:f32 = 0.0;
    for n in 0..=ix.len()-1 {
        let v = ix[n];
        for m in n+1..=ix.len()-1 {

            match v.cmp(&ix[m]){ 
                Ordering::Less =>  con +=1.0,
                Ordering::Greater => dis +=1.0,
                _ =>  break,
            };

        }
    }

    let n = ix.len() as f32;
    (con-dis) / ((n*(n-1.0))/2.0)
}
