use crate::ALLOCATED;
use crate::value::Value;
use crate::vm::Vm;
use std::sync::atomic::Ordering::Relaxed;

fn mark_roots(vm: &mut Vm) -> Vec<Value> {
    let mut gray_stack = Vec::new();

    // Marks instances on the stack.
    for val in vm.stack.iter() {
        match val {
            Value::Instance(instance) => {
                instance.is_marked.replace(true);
                gray_stack.push(val.clone());
            }
            _ => (),
        }
    }

    // Marks global instances.
    for val in vm.global_values.iter() {
        match val {
            Value::Instance(instance) => {
                instance.is_marked.replace(true);
                gray_stack.push(val.clone());
            }
            _ => (),
        }
    }

    gray_stack
}

fn trace_references(mut gray_stack: Vec<Value>) {
    while let Some(black) = gray_stack.pop() {
        match black {
            Value::Instance(instance) => {
                for val in instance.fields.borrow().values() {
                    match val {
                        Value::Instance(instance) => {
                            if !instance.is_marked.get() {
                                instance.is_marked.replace(true);
                                gray_stack.push(val.clone());
                            }
                        }
                        _ => (),
                    }
                }
            }
            _ => (),
        }
    }
}

fn sweep(vm: &mut Vm) {
    let mut swap_idx = vm.instances.len() - 1;
    let mut truncate_len = vm.instances.len();
    for cur_idx in (0..vm.instances.len()).rev() {
        let weak_ref = &vm.instances[cur_idx];
        match weak_ref.upgrade() {
            Some(instance) => {
                // If is_marked is false, then instance is in a reference cycle.
                if instance.is_marked.get() {
                    instance.is_marked.replace(false);
                } else {
                    // Drops all fields in instance to break the reference cycle.
                    instance.fields.take();
                    vm.instances.swap(swap_idx, cur_idx);
                    truncate_len -= 1;
                    swap_idx = swap_idx.saturating_sub(1);
                }
            }
            None => {
                // Remove invalid ref from instances Vec.
                vm.instances.swap(swap_idx, cur_idx);
                truncate_len -= 1;
                swap_idx = swap_idx.saturating_sub(1);
            }
        }
    }
    vm.instances.truncate(truncate_len);
}

pub fn collect_garbage(vm: &mut Vm) {
    let gray_stack = mark_roots(vm);
    trace_references(gray_stack);
    sweep(vm);
    vm.next_gc = ALLOCATED.load(Relaxed) * vm.gc_heap_grow_factor;
}
