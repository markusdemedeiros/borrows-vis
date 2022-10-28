struct Link {data: u32, next: Box<List>}
type List = Option<Link>;
fn main() {
    // Setup 
    let ll0: List = None;
    let ll1: List = Some(Link{next: Box::new(ll0), data: 5});
    let ll2: List = Some(Link{next: Box::new(ll1), data: 4});
    let ll3: List = Some(Link{next: Box::new(ll2), data: 3});
    let mut ll4: List = Some(Link{next: Box::new(ll3), data: 6});
    let l = &mut ll4;


    let mut x: &mut Link = match l {
        None => return,
        Some(lnk) => lnk,
    };
    
    while let Some(v) = &mut(*(*x).next) {
        if x.data > v.data {
            let tmp = x.data;
            x.data = v.data;
            v.data = tmp;
        }
        
        x = v;
    }
}
