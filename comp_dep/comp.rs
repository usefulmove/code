use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::num::ParseFloatError;
use std::path::Path;
use std::path::Display;
use std::collections::HashMap;

const COMP_VERSION: &str = env!("CARGO_PKG_VERSION");

/*

    note: base data structure is a vector (linked
    list) used as a stack. atoms on the list are
    either be symbols (commands) or values. each
    calculation is a list of operations that are
    processed in order of occurrence. this is an
    implementation of a lisp interpreter for rev-
    erse polish notation s-expressions (sexp).

      operations list structure
        (object : command or value)
        "5"
        "sqrt"
        "1
        "-"
        "2"
        "/"

    a list evaluation engine takes the list of
    strings and executes the corresponding oper-
    ations then returns the resulting mutated
    stack.

*/

// -- command list -------------------------------------------------------------

const CMDS: &str = "drop dup swap cls clr roll rot sa .a a sb .b b sc .c c + +_ \
- x x_ / chs abs round int inv sqrt throot proot ^ exp % mod ! gcd pi e dtor \
rtod sin asin cos acos tan atan log log10 ln";


fn main() {
  env::set_var("RUST_BACKTRACE", "0"); // enable or disable backtrace on error

  let mut args: Vec<String> = env::args().collect();

  // construct command interpreter
  let mut cinter = Interpreter::new();


  if args.len() <= 1 {
    args.push("help".to_string());
  }

  if args[1] == "--help" || args[1] == "help" {
    // display command usage information
    println!("usage: comp [version] [help]");
    println!("       comp <list>");
    println!("       comp -f <file>");
    println!();
    println!("where <list> represents a sequence of reverse Polish notion (RPN) \
    postfix operations or <file> is a file containing a similar sequence of \
    operations. Each operation must be either a command (symbol) or value. As \
    examples, 'comp 3 4 +' adds the values 3 and 4 and '3 dup x 4 dup x +' \
    computes the sum of the squares of 3 and 4. The available commands are \
    listed below.");
    println!();
    println!("commands:");
    println!("{CMDS}");
    std::process::exit(0);
  } else if args[1] == "--version" || args[1] == "version" {
    // display version information
    println!("comp {COMP_VERSION}");
    std::process::exit(0);
  } else if args[1] == "mona" {
    println!("{MONA}");
    std::process::exit(0);
  } else if args[1] == "-f" || args[1] == "--file" {
    // read operations list input from file
    if args.len() > 2 {
      let filename: String = args[2].to_string();
      let path: &Path = Path::new(&filename);
      let display: Display = path.display();

      // open file
      let mut file: File = match File::open(&path) {
        Ok(file) => file,
        Err(error) => {
          eprintln!("error: could not open [{display}] file: {error}");
          std::process::exit(99);
        },
      };

      // read file contents
      let mut file_contents: String = String::new();
      match file.read_to_string(&mut file_contents) {
        Ok(_) => (),
        Err(error) => {
          eprintln!("error: could not read [{display}]: {error}");
          std::process::exit(99);
        },
      };
  
      // split individual list elements
      let temp_ops: Vec<&str> = file_contents.split_whitespace().collect();
  
      // create operations list vector
      for op in temp_ops {
        cinter.ops.push(op.to_string());
      }
    } else {
      eprintln!("error: no file path was passed");
      std::process::exit(99);
    }
  } else {
    // read operations list input from command line arguments
    cinter.ops = (&args[1..]).to_vec();
  }

  // process operations list
  cinter.process_ops();

  // display resulting computation stack
  for element in cinter.stack {
    println!("{element}");
  }

  std::process::exit(0);
}

struct Interpreter {
  stack: Vec<f64>,
  mem_a: f64,
  mem_b: f64,
  mem_c: f64,
  ops: Vec<String>,
  fns: Vec<Function>,
  cmap: HashMap<String, fn(&mut Interpreter)>,
}

struct Function {
  name: String,
  fops: Vec<String>,
}

impl Interpreter {
  // constructor
  fn new() -> Interpreter {
    let mut p = Interpreter {
      stack: Vec::new(),
      mem_a: 0.0,
      mem_b: 0.0,
      mem_c: 0.0,
      ops: Vec::new(),
      fns: Vec::new(),
      cmap: HashMap::new(),
    };

    p.construct();

    p
  }

  fn process_ops(&mut self) {
    while !self.ops.is_empty() {
      let operation: String = self.ops.remove(0); // pop first operation
      self.process_node(operation.as_str());
    }
  }

  fn add_command(&mut self, name: &str, func: fn(&mut Interpreter)) {
    self.cmap.insert(name.to_string(), func);
  }

  fn construct(&mut self) {
    // stack manipulation
    self.add_command("drop",   Interpreter::c_drop);     // drop
    self.add_command("dup",    Interpreter::c_dup);      // duplicate
    self.add_command("swap",   Interpreter::c_swap);     // swap x and y
    self.add_command("cls",    Interpreter::c_cls);      // clear stack
    self.add_command("clr",    Interpreter::c_cls);      // clear stack
    self.add_command("roll",   Interpreter::c_roll);     // roll stack
    self.add_command("rot",    Interpreter::c_rot);      // rotate stack (reverse direction from roll)
    // memory usage
    self.add_command("sa",     Interpreter::c_store_a);  // store (pop value off stack and store)
    self.add_command(".a",     Interpreter::c_store_a);  // store (pop value off stack and store)
    self.add_command("a",      Interpreter::c_push_a);   // retrieve (push stored value onto the stack)
    self.add_command("sb",     Interpreter::c_store_b);  // store
    self.add_command(".b",     Interpreter::c_store_b);  // store
    self.add_command("b",      Interpreter::c_push_b);   // retrieve
    self.add_command("sc",     Interpreter::c_store_c);  // store
    self.add_command(".c",     Interpreter::c_store_c);  // store
    self.add_command("c",      Interpreter::c_push_c);   // retrieve
    // math operations
    self.add_command("+",      Interpreter::c_add);      // add
    self.add_command("+_",     Interpreter::c_add_all);  // add all
    self.add_command("-",      Interpreter::c_sub);      // subtract
    self.add_command("x",      Interpreter::c_mult);     // multiply
    self.add_command("x_",     Interpreter::c_mult_all); // multiply all
    self.add_command("/",      Interpreter::c_div);      // divide
    self.add_command("chs",    Interpreter::c_chs);      // change sign
    self.add_command("abs",    Interpreter::c_abs);      // absolute value
    self.add_command("round",  Interpreter::c_round);    // round
    self.add_command("int",    Interpreter::c_round);
    self.add_command("inv",    Interpreter::c_inv);      // invert (1/x)
    self.add_command("sqrt",   Interpreter::c_sqrt);     // square root
    self.add_command("throot", Interpreter::c_throot);   // nth root
    self.add_command("proot",  Interpreter::c_proot);    // find principal roots
    self.add_command("^",      Interpreter::c_exp);      // exponenation
    self.add_command("exp",    Interpreter::c_exp);
    self.add_command("%",      Interpreter::c_mod);      // modulus
    self.add_command("mod",    Interpreter::c_mod);
    self.add_command("!",      Interpreter::c_fact);     // factorial
    self.add_command("gcd",    Interpreter::c_gcd);      // greatest common divisor
    self.add_command("pi",     Interpreter::c_pi);       // pi
    self.add_command("e",      Interpreter::c_euler);    // Euler's constant
    self.add_command("dtor",   Interpreter::c_dtor);     // degrees to radians
    self.add_command("rtod",   Interpreter::c_rtod);     // radians to degrees
    self.add_command("sin",    Interpreter::c_sin);      // sine
    self.add_command("asin",   Interpreter::c_asin);     // arcsine
    self.add_command("cos",    Interpreter::c_cos);      // cosine
    self.add_command("acos",   Interpreter::c_acos);     // arccosine
    self.add_command("tan",    Interpreter::c_tan);      // tangent
    self.add_command("atan",   Interpreter::c_atan);     // arctangent
    self.add_command("log",    Interpreter::c_log10);    // log (base 10)
    self.add_command("log10",  Interpreter::c_log10);
    self.add_command("ln",     Interpreter::c_ln);       // natural log
    // control flow
    self.add_command("fn",     Interpreter::c_fn);       // function definition
    self.add_command("(",      Interpreter::c_comment);  // function definition
  }

  fn process_node(&mut self, op: &str) {
    if self.cmap.contains_key(op) {
      let f = self.cmap[op];
      f(self);
    } else {
      let ind: i32 = self.is_user_function(op);
      if ind != -1 { // user-defined function?
        // copy user function ops (fops) into main ops
        for i in (0..self.fns[ind as usize].fops.len()).rev() {
          let fop: String = self.fns[ind as usize].fops[i].clone();
          self.ops.insert(0, fop);
        }

      } else {
        let res: Result<f64, ParseFloatError> = self.parse_value(op);
        
        let val = match res {
          Ok(val) => val,
          Err(_error) => {
            eprintln!("error: comp interpreter was passed an unknown \
                       expression: [{op}] is not a recognized operation \
                       or value");
            std::process::exit(99);
          },
        };

        self.stack.push(val);
      }
    }
  }

  fn parse_value(&self, op: &str) -> Result<f64, ParseFloatError> {
    let value: f64 = op.parse::<f64>()?;
    Ok(value)
  }

  // -- command functions ------------------------------------------------------
  
  // ---- stack manipulation ---------------------------------------------------
  
  fn c_drop(&mut self) {
    self.stack.pop().unwrap();
  }
  
  fn c_dup(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack.push(self.stack[end]);
  }
  
  fn c_swap(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack.swap(end, end-1);
  }
  
  fn c_cls(&mut self) {
    self.stack.clear();
  }
  
  fn c_roll(&mut self) {
    let o: f64 = self.stack.pop().unwrap(); // remove last
    self.stack.splice(0..0, [o]);           // add as first
  }
  
  fn c_rot(&mut self) {
    let o: f64 = self.stack.remove(0); // remove first
    self.stack.push(o);                // add as last
  }
  
  
  // ---- memory usage ---------------------------------------------------------
  
  fn c_store_a(&mut self) {
    self.mem_a = self.stack.pop().unwrap();
  }
  
  fn c_push_a(&mut self) {
    self.stack.push(self.mem_a);
  }
  
  fn c_store_b(&mut self) {
    self.mem_b = self.stack.pop().unwrap();
  }
  
  fn c_push_b(&mut self) {
    self.stack.push(self.mem_b);
  }
  
  fn c_store_c(&mut self) {
    self.mem_c = self.stack.pop().unwrap();
  }
  
  fn c_push_c(&mut self) {
    self.stack.push(self.mem_c);
  }
  
  
  // -- math operations --------------------------------------------------------
  
  fn c_add(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end-1] += self.stack.pop().unwrap();
  }
  
  fn c_add_all(&mut self) {
    while self.stack.len() > 1 {
      let end: usize = self.stack.len() - 1;
      self.stack[end-1] += self.stack.pop().unwrap();
    }
  }
  
  fn c_sub(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end-1] -= self.stack.pop().unwrap();
  }
  
  fn c_mult(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end-1] *= self.stack.pop().unwrap();
  }
  
  fn c_mult_all(&mut self) {
    while self.stack.len() > 1 {
      let end: usize = self.stack.len() - 1;
      self.stack[end-1] *= self.stack.pop().unwrap();
    }
  }
  
  fn c_div(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end-1] /= self.stack.pop().unwrap();
  }
  
  fn c_chs(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] *= -1.0;
  }
  
  fn c_abs(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = f64::abs(self.stack[end]);
  }
  
  fn c_round(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = self.stack[end].round();
  }
  
  fn c_inv(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = 1.0 / self.stack[end];
  }
  
  fn c_sqrt(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = f64::sqrt(self.stack[end]);
  }
  
  fn c_throot(&mut self) {
    let end: usize = self.stack.len() - 1;
    let o: f64 = self.stack.pop().unwrap();
    self.stack[end-1] = self.stack[end-1].powf(1.0/o);
  }
  
  fn c_proot(&mut self) {
    let c: f64 = self.stack.pop().unwrap();
    let b: f64 = self.stack.pop().unwrap();
    let a: f64 = self.stack.pop().unwrap();
  
    if (b*b - 4.0*a*c) < 0.0 {
      self.stack.push(-1.0*b/(2.0*a)); // root1 real
      self.stack.push(f64::sqrt(4.0*a*c-b*b)/(2.0*a)); // root1 imag
      self.stack.push(-1.0*b/(2.0*a)); // root2 real
      self.stack.push(-1.0*f64::sqrt(4.0*a*c-b*b)/(2.0*a)); // root2 imag
    } else {
      self.stack.push(-1.0*b+f64::sqrt(b*b-4.0*a*c)/(2.0*a)); // root1 real
      self.stack.push(0.0); // root1 imag
      self.stack.push(-1.0*b-f64::sqrt(b*b-4.0*a*c)/(2.0*a)); // root2 real
      self.stack.push(0.0); // root2 imag
    }
  }
  
  fn c_exp(&mut self) {
    let end: usize = self.stack.len() - 1;
    let o: f64 = self.stack.pop().unwrap();
    self.stack[end-1] = self.stack[end-1].powf(o);
  }
  
  fn c_mod(&mut self) {
    let end: usize = self.stack.len() - 1;
    let o: f64 = self.stack.pop().unwrap();
    self.stack[end-1] %= o;
  }
  
  fn c_fact(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = factorial(self.stack[end] as u64) as f64;
  }
  
  fn c_gcd(&mut self) {
    let a: u64 = self.stack.pop().unwrap() as u64;
    let b: u64 = self.stack.pop().unwrap() as u64;
    let g: f64 = gcd(a,b) as f64;
    self.stack.push(g);
  }
  
  fn c_pi(&mut self) {
    self.stack.push(std::f64::consts::PI);
  }
  
  fn c_euler(&mut self) {
    self.stack.push(std::f64::consts::E);
  }
  
  fn c_dtor(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = self.stack[end].to_radians();
  }
  
  fn c_rtod(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = self.stack[end].to_degrees();
  }
  
  fn c_sin(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = self.stack[end].sin();
  }
  
  fn c_asin(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = self.stack[end].asin();
  }
  
  fn c_cos(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = self.stack[end].cos();
  }
  
  fn c_acos(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = self.stack[end].acos();
  }
  
  fn c_tan(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = self.stack[end].tan();
  }
  
  fn c_atan(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = self.stack[end].atan();
  }
  
  fn c_log10(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = self.stack[end].log10();
  }
  
  fn c_ln(&mut self) {
    let end: usize = self.stack.len() - 1;
    self.stack[end] = self.stack[end].ln();
  }


  // -- control flow -----------------------------------------------------------
  
  fn c_fn(&mut self) {
    // get function name
    let fn_name: String = self.ops.remove(0);

    // create new function instance and assign function name
    self.fns.push(Function { name: fn_name,
                             fops: Vec::new(),
                           });
    let fpos: usize = self.fns.len() - 1; // added function position in function vector

    // build out function operations my reading from interpreter ops
    while self.ops[0] != "end" {
      self.fns[fpos].fops.push(self.ops.remove(0));
    }
    self.ops.remove(0); // remove "end" op
  }

  fn is_user_function(&self, op: &str) -> i32 {
    if !self.fns.is_empty() {
      for i in 0..self.fns.len() {
        if self.fns[i].name == op {
          return i as i32;
        }
      }
    }
    -1
  }

  fn c_comment(&mut self) {
    // ignore all elements between "(" and ")"
    while self.ops[0] != ")" {
      self.ops.remove(0);
    }
    self.ops.remove(0); // remove ")" op
  }

}


// -- support functions --------------------------------------------------------

fn factorial(n: u64) -> u64 {
  if n < 2 {
    1
  } else {
    n * factorial(n-1)
  }
}

fn gcd(a: u64, b: u64) -> u64 {
  if b != 0 {
    gcd(b, a % b)
  } else {
    a
  }
}


// -- mona ---------------------------------------------------------------------

const MONA: &str = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!>''''''<!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!'''''`             ``'!!!!!!!!!!!!!!!!!!!!!!!!\n\
       !!!!!!!!!!!!!!!!!!!!!!!!''`          .....         `'!!!!!!!!!!!!!!!!!!!!!\n\
       !!!!!!!!!!!!!!!!!!!!!'`      .      :::::'            `'!!!!!!!!!!!!!!!!!!\n\
       !!!!!!!!!!!!!!!!!!!'     .   '     .::::'                `!!!!!!!!!!!!!!!!\n\
       !!!!!!!!!!!!!!!!!'      :          `````                   `!!!!!!!!!!!!!!\n\
       !!!!!!!!!!!!!!!!        .,cchcccccc,,.                       `!!!!!!!!!!!!\n\
       !!!!!!!!!!!!!!!     .-\"?$$$$$$$$$$$$$$c,                      `!!!!!!!!!!!\n\
       !!!!!!!!!!!!!!    ,ccc$$$$$$$$$$$$$$$$$$$,                     `!!!!!!!!!!\n\
       !!!!!!!!!!!!!    z$$$$$$$$$$$$$$$$$$$$$$$$;.                    `!!!!!!!!!\n\
       !!!!!!!!!!!!    <$$$$$$$$$$$$$$$$$$$$$$$$$$:.                    `!!!!!!!!\n\
       !!!!!!!!!!!     $$$$$$$$$$$$$$$$$$$$$$$$$$$h;:.                   !!!!!!!!\n\
       !!!!!!!!!!'     $$$$$$$$$$$$$$$$$$$$$$$$$$$$$h;.                   !!!!!!!\n\
       !!!!!!!!!'     <$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$                   !!!!!!!\n\
       !!!!!!!!'      `$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$F                   `!!!!!!\n\
       !!!!!!!!        c$$$$???$$$$$$$P\"\"  \"\"\"??????\"                      !!!!!!\n\
       !!!!!!!         `\"\" .,.. \"$$$$F    .,zcr                            !!!!!!\n\
       !!!!!!!         .  dL    .?$$$   .,cc,      .,z$h.                  !!!!!!\n\
       !!!!!!!!        <. $$c= <$d$$$   <$$$$=-=+\"$$$$$$$                  !!!!!!\n\
       !!!!!!!         d$$$hcccd$$$$$   d$$$hcccd$$$$$$$F                  `!!!!!\n\
       !!!!!!         ,$$$$$$$$$$$$$$h d$$$$$$$$$$$$$$$$                   `!!!!!\n\
       !!!!!          `$$$$$$$$$$$$$$$<$$$$$$$$$$$$$$$$'                    !!!!!\n\
       !!!!!          `$$$$$$$$$$$$$$$$\"$$$$$$$$$$$$$P>                     !!!!!\n\
       !!!!!           ?$$$$$$$$$$$$??$c`$$$$$$$$$$$?>'                     `!!!!\n\
       !!!!!           `?$$$$$$I7?\"\"    ,$$$$$$$$$?>>'                       !!!!\n\
       !!!!!.           <<?$$$$$$c.    ,d$$?$$$$$F>>''                       `!!!\n\
       !!!!!!            <i?$P\"??$$r--\"?\"\"  ,$$$$h;>''                       `!!!\n\
       !!!!!!             $$$hccccccccc= cc$$$$$$$>>'                         !!!\n\
       !!!!!              `?$$$$$$F\"\"\"\"  `\"$$$$$>>>''                         `!!\n\
       !!!!!                \"?$$$$$cccccc$$$$??>>>>'                           !!\n\
       !!!!>                  \"$$$$$$$$$$$$$F>>>>''                            `!\n\
       !!!!!                    \"$$$$$$$$???>'''                                !\n\
       !!!!!>                     `\"\"\"\"\"                                        `\n\
       !!!!!!;                       .                                          `\n\
       !!!!!!!                       ?h.\n\
       !!!!!!!!                       $$c,\n\
       !!!!!!!!>                      ?$$$h.              .,c\n\
       !!!!!!!!!                       $$$$$$$$$hc,.,,cc$$$$$\n\
       !!!!!!!!!                  .,zcc$$$$$$$$$$$$$$$$$$$$$$\n\
       !!!!!!!!!               .z$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n\
       !!!!!!!!!             ,d$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$          .\n\
       !!!!!!!!!           ,d$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$         !!\n\
       !!!!!!!!!         ,d$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$        ,!'\n\
       !!!!!!!!>        c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$.\n\
       !!!!!!''       ,d$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$      allen mullen";


// -----------------------------------------------------------------------------
// -- unit and regression tests ------------------------------------------------

#[cfg(test)]

mod comp_tests {

  #[test]
  fn test_core() {
    let mut test_cinter = super::Interpreter::new();

    test_cinter.stack.push(1.0);
    test_cinter.stack.push(2.0);
    test_cinter.stack.push(3.0);
    test_cinter.stack.push(4.0);

    test_cinter.c_rot();
    test_cinter.c_rot();
    test_cinter.c_roll();
    test_cinter.c_roll();

    test_cinter.c_dtor();
    test_cinter.c_cos();
    test_cinter.c_acos();
    test_cinter.c_sin();
    test_cinter.c_asin();
    test_cinter.c_tan();
    test_cinter.c_atan();
    test_cinter.c_rtod();
    test_cinter.c_round();
    test_cinter.c_roll();
    test_cinter.c_roll();
    test_cinter.c_roll();
    test_cinter.c_roll();
    test_cinter.c_dup();
    test_cinter.c_drop();
    test_cinter.c_swap();
    test_cinter.c_swap();
    test_cinter.c_add();
    test_cinter.c_sub();
    test_cinter.c_div();

    assert!(test_cinter.stack.pop().unwrap() == -0.2);
  }

  #[test]
  fn test_support() {
    assert!(super::gcd(55, 10) == 5);
    assert!(super::factorial(10) == 3628800);
  }

  #[test]
  fn test_roots() {
    let mut test_cinter = super::Interpreter::new();

    test_cinter.stack.push(2.0);
    test_cinter.c_dup();
    test_cinter.c_sqrt();
    test_cinter.c_swap();
    test_cinter.stack.push(32.0);
    test_cinter.c_exp();
    test_cinter.stack.push(32.0 * 2.0);
    test_cinter.c_throot();

    assert!(test_cinter.stack.pop().unwrap() == test_cinter.stack.pop().unwrap());

    test_cinter.stack.push(1.0);
    test_cinter.stack.push(-2.0);
    test_cinter.c_chs();
    test_cinter.c_chs();
    test_cinter.c_pi();
    test_cinter.c_mult();
    test_cinter.c_pi();
    test_cinter.stack.push(2.0);
    test_cinter.c_exp();
    test_cinter.stack.push(1.0);
    test_cinter.c_add();
    test_cinter.c_proot();
    test_cinter.c_add_all();
    test_cinter.stack.push(2.0);
    test_cinter.c_div();
    test_cinter.c_pi();

    assert!(test_cinter.stack.pop().unwrap() == test_cinter.stack.pop().unwrap());
  }

  #[test]
  #[should_panic]
  fn test_cls() {
    let mut test_cinter = super::Interpreter::new();

    test_cinter.stack.push(1.0);
    test_cinter.stack.push(2.0);
    test_cinter.stack.push(3.0);
    test_cinter.stack.push(4.0);
    test_cinter.stack.push(1.0);
    test_cinter.stack.push(2.0);
    test_cinter.stack.push(3.0);
    test_cinter.stack.push(4.0);
    test_cinter.stack.push(1.0);
    test_cinter.stack.push(2.0);
    test_cinter.stack.push(3.0);
    test_cinter.stack.push(4.0);
    test_cinter.c_cls();

    assert!(test_cinter.stack.pop().unwrap() == 0.0);
  }

  #[test]
  fn test_mem() {
    let mut test_cinter = super::Interpreter::new();

    test_cinter.stack.push(1.0);
    test_cinter.stack.push(2.0);
    test_cinter.stack.push(3.0);
    test_cinter.stack.push(4.0);
    test_cinter.stack.push(1.0);
    test_cinter.stack.push(2.0);
    test_cinter.stack.push(3.0);
    test_cinter.stack.push(4.0);
    test_cinter.stack.push(1.0);
    test_cinter.stack.push(2.0);
    test_cinter.stack.push(3.0);
    test_cinter.stack.push(4.0);
    test_cinter.c_chs();
    test_cinter.c_abs();
    test_cinter.c_inv();
    test_cinter.c_inv();
    test_cinter.c_pi();
    test_cinter.c_euler();
    test_cinter.stack.push(0.0);
    test_cinter.c_store_b(); // 0
    test_cinter.c_store_a(); // e
    test_cinter.c_store_c(); // pi
    test_cinter.c_cls();
    test_cinter.c_push_b(); // 0
    test_cinter.c_push_c(); // pi
    test_cinter.c_add();
    test_cinter.c_push_a(); // e
    test_cinter.c_add();

    assert!(test_cinter.stack.pop().unwrap() == std::f64::consts::PI + std::f64::consts::E);
  }

  #[test]
  fn test_cmp() {
    let mut test_cinter = super::Interpreter::new();

    test_cinter.stack.push(10.0);
    test_cinter.c_log10();
    test_cinter.c_euler();
    test_cinter.c_ln();
    test_cinter.stack.push(105.0);
    test_cinter.stack.push(2.0);
    test_cinter.c_mod();
    test_cinter.stack.push(3049.0);
    test_cinter.stack.push(1009.0);
    test_cinter.c_gcd();
    test_cinter.c_mult_all();

    assert!(test_cinter.stack.pop().unwrap() == 1.0);

    test_cinter.stack.push(20.0);
    test_cinter.c_fact();

    assert!(test_cinter.stack.pop().unwrap() == 2432902008176640000.0);
  }
}
