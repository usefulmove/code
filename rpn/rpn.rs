use std::env;
use std::collections::LinkedList;

fn main() {
  let args: Vec<String> = env::args().collect();
  println!("{:?}", args);

  ops = args;

  let mut stack = LinkedList::new();

  stack = evaluate_list(stack, ops);

  enum Command {
    CNULL,
    CADD
  }
}

fn evaluate_list(stack_in: LinkedList, Vec<&'oplist str>) -> LinkedList {
  stack_out = stack_in;

  for cov in oplist {
    stack_out = process_node(stack_out, cov);
  }
}

fn process_node(stack_in: LinkedList, cmd_or_val: &str) -> LinkedList {
  stack_out = stack_in;

  command_id = iscommand(cmd_or_val);

  if TODO { // command
    stack_out = execute_command(stack_out, command_id);
  } else { // value
    stack_out.push_back(cmd_or_val as f32);
  }

  return stack_out
}

fn iscommand(sinput: &str) -> Command {
  if sinput == ":+" {
    return Command::CADD;
  }
}

fn execute_command(stack_in: LinkedList, command_id: Command) -> LinkedList {
  o = stack_in;

  if command_id == Command::CADD {
    // TODO pop end of stack and add to next one
    x = o.pop_back();
    y = o.pop_back();

    o.push_back(x + y);
  }

  return o
}
