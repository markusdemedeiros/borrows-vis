struct K { data: usize }

fn main() {
    let mut k1: K = K { data: 10 };
    let mut k2: K = K { data: 11 };
    
    let mut bk = &mut k1;
    let mut bk2 = &mut k2;

    if (1 == 1) {
        bk = bk2;
    }

    let bk_test = &mut (*bk);
}
