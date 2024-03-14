fn assert(cond: bool, msg: str) {
    if !cond {
        panic(msg);
    }
}

fn main() {
    println("\nhash: ");
    {
        println("running map()...");
        hash::map();

        println("running set()...");
        hash::set();

        println("running map_builtin()...");
        hash::map_builtin();
    }

    println("\narray: ");
    {
        println("pattern_stuff()...");
        array::pattern_stuff();
    
        println("nested_ptr()...");
        array::nested_ptr();

        println("nested_destructure_1()...");
        array::nested_destructure_1();

        println("nested_destructure_2()...");
        array::nested_destructure_2();

        println("nested_destructure_3()...");
        array::nested_destructure_3();

        println("nested_destructure_4()...");
        array::nested_destructure_4();

        println("nested_destructure_5()...");
        array::nested_destructure_5();
    }

    println("\npatterns: ");
    {
        println("struct_pattern()...");
        patterns::struct_pattern();

        println("union_pattern()...");
        patterns::union_pattern();

        println("union_struct_pattern()...");
        patterns::union_struct_pattern();

        println("option_struct_pattern()...");
        patterns::option_struct_pattern();

        println("integer_pattern()...");
        patterns::integer_pattern();

        println("string_pattern()...");
        patterns::string_pattern();
    }

    println("\nloops: ");
    {
        println("while_loop()...");
        loops::while_loop();

        println("for_loop()...");
        loops::for_loop();

        println("infinite_loop()...");
        loops::infinite_loop();
    }

    println("\ndynamic dispatch: ");
    {
        println("normal()...");
        traits::dd::normal();

        println("dependant()...");
        traits::dd::dependant();

        println("recursive()...");
        traits::dd::recursive();
    }

    println("\ntrait default: ");
    {
        println("nooverride_direct()...");
        traits::default::nooverride_direct();

        println("nooverride_dyn()...");
        traits::default::nooverride_dyn();

        println("nooverride_generic()...");
        traits::default::nooverride_generic();

        println("override_direct()...");
        traits::default::override_direct();

        println("override_dyn()...");
        traits::default::override_dyn();

        println("override_generic()...");
        traits::default::override_generic();
    }

    println("\ntrait default by extension: ");
    {
        println("nooverride_direct()...");
        traits::default_ext::nooverride_direct();

        println("nooverride_dyn()...");
        traits::default_ext::nooverride_dyn();

        println("nooverride_generic()...");
        traits::default_ext::nooverride_generic();

        println("override_direct()...");
        traits::default_ext::override_direct();

        println("override_dyn()...");
        traits::default_ext::override_dyn();

        println("override_generic()...");
        traits::default_ext::override_generic();
    }

    println("\noperator overloading: ");
    {
        println("cmp_eq()...");
        ops::cmp_eq();
    }

    println("\nall tests passed!");
}
