// TODO: support marshaller.

use std::mem;
use std::ptr;

use libc::{c_uint, c_void};

use gobject_ffi;
use translate::{FromGlibPtrFull, FromGlibPtrNone, Stash, StashMut, ToGlibPtr, ToGlibPtrMut, Uninitialized};
use Value;
use ToValue;

pub struct Closure {
    closure: *mut gobject_ffi::GClosure,
}

impl Closure {
    pub fn new<F: Fn(&[Value]) -> Option<Value> + 'static>(callback: F) -> Self {
        unsafe extern "C" fn marshal<F: Fn(&[Value]) -> Option<Value> + 'static>(_closure: *mut gobject_ffi::GClosure,
            return_value: *mut gobject_ffi::GValue, n_param_values: c_uint, mut param_values: *mut gobject_ffi::GValue,
            _invocation_hint: *mut c_void, marshal_data: *mut c_void)
        {
            let mut values = vec![];
            for _ in 0 .. n_param_values {
                let value: Value = FromGlibPtrNone::from_glib_none(param_values);
                values.push(value);
                param_values = param_values.offset(1);
            }
            let callback: &Box<F> = mem::transmute(marshal_data as *mut _);
            let result = callback(&values);
            match result {
                Some(result) => *return_value = result.into_raw(),
                None => {
                    let result = Value::uninitialized();
                    let mut gvalue = result.into_raw();
                    gvalue.g_type = gobject_ffi::G_TYPE_NONE;
                    *return_value = gvalue;
                },
            }
            // TODO: remove this.
            for value in values {
                mem::forget(value);
            }
        }

        unsafe extern "C" fn finalize<F: Fn(&[Value]) -> Option<Value> + 'static>(notify_data: *mut c_void,
            _closure: *mut gobject_ffi::GClosure)
        {
            let _callback: Box<Box<F>> = Box::from_raw(notify_data as *mut _);
            // callback is dropped here.
        }

        unsafe {
            let size = 4 + 4 + 3 * mem::size_of::<*mut c_void>() as u32;
            let closure = gobject_ffi::g_closure_new_simple(size, ptr::null_mut());
            assert_ne!(closure, ptr::null_mut());
            let callback = Box::new(Box::new(callback));
            let ptr: *mut Box<F> = Box::into_raw(callback);
            let ptr: *mut c_void = ptr as *mut _;
            gobject_ffi::g_closure_set_meta_marshal(closure, ptr, Some(marshal::<F>));
            gobject_ffi::g_closure_add_finalize_notifier(closure, ptr, Some(finalize::<F>));
            Closure {
                closure: closure,
            }
        }
    }

    pub fn invoke(&mut self, values: &[&ToValue]) -> Option<Value> {
        let mut result: gobject_ffi::GValue = unsafe { mem::zeroed() };
        let values: Vec<_> = values.iter()
            .map(|v| v.to_value())
            .collect();
        let mut gvalues: Vec<gobject_ffi::GValue> = values.iter()
            .map(|v| unsafe { ptr::read(v.to_glib_none().0) })
            .collect();
        unsafe {
            gobject_ffi::g_closure_invoke(self.to_glib_none_mut().0, &mut result, gvalues.len() as u32,
                gvalues.as_mut_ptr(), ptr::null_mut());
        }
        if result.g_type == gobject_ffi::G_TYPE_NONE {
            None
        }
        else {
            Some(unsafe { Value::from_glib_full(&mut result as *mut _) })
        }
    }
}

impl Clone for Closure {
    fn clone(&self) -> Self {
        unsafe { gobject_ffi::g_closure_ref(self.closure); }
        Closure { closure: self.closure }
    }
}

impl Drop for Closure {
    fn drop(&mut self) {
        unsafe { gobject_ffi::g_closure_unref(self.closure) };
    }
}

impl<'a> ToGlibPtr<'a, *const gobject_ffi::GClosure> for Closure {
    type Storage = &'a Closure;

    fn to_glib_none(&'a self) -> Stash<'a, *const gobject_ffi::GClosure, Self> {
        Stash(self.closure, self)
    }
}

impl<'a> ToGlibPtrMut<'a, *mut gobject_ffi::GClosure> for Closure {
    type Storage = &'a mut Closure;

    fn to_glib_none_mut(&'a mut self) -> StashMut<'a, *mut gobject_ffi::GClosure, Self> {
        StashMut(self.closure, self)
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use std::sync::atomic::{AtomicUsize, Ordering};

    use super::Closure;
    use ToValue;
    use Value;

    fn closure_fn(values: &[Value]) -> Option<Value> {
        assert_eq!(values.len(), 2);
        let string: Option<String> = values[0].get();
        assert_eq!(string, Some("test".to_string()));
        let int: Option<i32> = values[1].get();
        assert_eq!(int, Some(42));
        Some(24.to_value())
    }

    #[test]
    fn test_closure() {
        let call_count = Rc::new(AtomicUsize::new(0));

        let count = call_count.clone();
        let mut closure = Closure::new(move |values| {
            count.fetch_add(1, Ordering::Relaxed);
            assert_eq!(values.len(), 2);
            let string: Option<String> = values[0].get();
            assert_eq!(string, Some("test".to_string()));
            let int: Option<i32> = values[1].get();
            assert_eq!(int, Some(42));
            None
        });
        let result = closure.invoke(&[&"test".to_string(), &42]);
        assert!(result.is_none());
        assert_eq!(call_count.load(Ordering::Relaxed), 1);

        let result = closure.invoke(&[&"test".to_string(), &42]);
        assert!(result.is_none());
        assert_eq!(call_count.load(Ordering::Relaxed), 2);

        let mut closure = Closure::new(closure_fn);
        let result = closure.invoke(&[&"test".to_string(), &42]);
        let int: Option<i32> = result.and_then(|result| result.get());
        assert_eq!(int, Some(24));
    }
}
