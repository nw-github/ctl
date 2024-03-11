fn assert(cond: bool, msg: str) {
    if !cond {
        panic(msg);
    }
}

fn main() {
    println("span: ");
    {
        println("running ranges()...");
        span::ranges();

        println("pattern()...");
        span::pattern();

        println("pattern_destructure()...");
        span::pattern_destructure();

        println("pattern_destructure_2()...");
        span::pattern_destructure_2();
    }

    println("\nvec: ");
    {
        println("running init1()...");
        vec::init1();

        println("running init2()...");
        vec::init2();
        
        println("running push_pop()...");
        vec::push_pop();

        println("running insert()...");
        vec::insert();

        println("running remove()...");
        vec::remove();

        println("running swap_remove()...");
        vec::swap_remove();
    }

    println("\nhash: ");
    {
        println("running map()...");
        hash::map();

        println("running set()...");
        hash::set();

        println("running map_builtin()...");
        hash::map_builtin();
    }

    println("\norder of evaluation: ");
    {
        println("blocks(trigger_else: false)...");
        ordering::blocks(false);

        println("blocks(trigger_else: true)...");
        ordering::blocks(true);

        println("positional_call()...");
        ordering::positional_call();

        println("increment()...");
        ordering::increment();

        println("instance_ordering()...");
        ordering::instance_ordering();

        println("keyword_call()...");
        ordering::keyword_call();
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

    println("\nmisc: ");
    {
        println("booleans()...");
        misc::booleans();

        println("positional::start()...");
        positional::start();

        println("positional::middle()...");
        positional::middle();

        println("positional::end()...");
        positional::end();

        println("struct_order()...");
        struct_order::test();

        println("modules()...");
        modules::test();

        println("statics()...");
        misc::statics();

        println("void_assigns()...");
        misc::void_assigns();

        println("sizes()...");
        misc::sizes();
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
