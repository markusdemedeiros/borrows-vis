struct Node {data: u32, left: Box<Tree>, right: Box<Tree> }
type Tree = Option<Node>;

fn main() {
    // Setup 
    let bt0: Node = Node{data: 10, left: Box::new(None), right: Box::new(None)};
    let bt1: Node = Node{data: 15, left: Box::new(None), right: Box::new(None)};
    let bt2: Node = Node{data: 13, left: Box::new(Some(bt0)), right: Box::new(Some(bt1))};
    let bt3: Node = Node{data: 20, left: Box::new(None), right: Box::new(None)};
    let mut bt4: Node = Node{data: 17, left: Box::new(Some(bt2)), right: Box::new(Some(bt3))};
    
    //       (17) 
    //      /   |
    //    (13) (20)
    //   /   |
    // (10) (15)

    let mut current = &mut bt4;
    let target: u32 = 15;

    // binary tree search
    loop {
        if (current.data > target) {
            if let Some(v) = &mut(*(*current).left) {
                current = v;
            } else {
                break;
            }
        } else if (current.data < target) {
            if let Some(v) = &mut(*(*current).right) {
                current = v;
            } else {
                break;
            }
        } else {
            break;
        }
    }

    let current_liveness = &mut (current.data);
}

