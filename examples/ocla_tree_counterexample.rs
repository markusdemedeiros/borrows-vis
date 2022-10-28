struct K { data: usize }

fn main() {
    let mut k1: K = K { data: 10 };
    let mut k2: K = K { data: 11 };
    let mut k3: K = K { data: 11 };
    
    let mut x = &mut k1; 
    let mut y = &mut k2; 
    let mut z = &mut k3; 

    if (1 == 1) {
        y = &mut x;
    }

    if (1 == 1) {
        y = &mut x;
    }

    let y_test = y;
    let z_test = z;
    let x_test = x;
}
