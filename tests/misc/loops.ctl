use super::assert;

pub fn while_loop() {
    {
        mut x = 0;
        let y = while x < 10 {
            if x == 5 {
                break x;
            }
            x++;
        };

        assert(y! == 5, "y was not equal to 5");
    }
    {
        mut x = 0;
        let y = while x < 2 {
            if x == 5 {
                break x;
            }
            x++;
        };

        assert(y is null, "y was not null");
    }
}

pub fn for_loop() {
    {
        mut x = @[1, 2, 3, 4, 5];
        let y = for i in x.iter_mut() {
            if *i == 3 {
                break i;
            }
        };

        let y = y!;
        assert(*y == 3, "y was not equal to 3");

        *y = 10;
        assert(*x.get(2)! == 10, "y was not a pointer into x"); 
    }

    {
        mut x = @[1, 2, 3, 4, 5];
        let y = for i in x.iter() {
            if *i == 3 {
                break *i;
            }
        };

        assert(y! == 3, "y was not equal to 3");
    }

    {
        mut x = @[1, 2, 3, 4, 5];
        let y = for i in x.iter_mut() {
            if *i == 5000 {
                break i;
            }
        };

        assert(y is null, "y was not null");
    }
}

pub fn infinite_loop() {
    let x = loop {
        break 10;
    };

    assert(x == 10, "x was not 10");
}
