# Ruby Blocks with Lisp Macros

**Jul 23, 2017**

The ruby block and yield syntax is really useful for hiding away setup and
cleanup code. For example, we can write `with_open_socket` which opens a TCP
connection, yields to the caller and then closes the connection as cleanup
after the caller is finished.

```ruby
require 'socket'

def with_open_socket(addr, port)
  socket = TCPSocket.new(addr, port)
  yield socket
  socket.close
end
```

This makes it easy for us to use a TCP client without worrying about how to
setup or cleanup.

```ruby
with_open_socket('www.google.com', 80) do |socket|
  socket.write("GET / HTTP/1.1\r\n\r\n")
  socket.gets
end

=> "HTTP/1.1 302 Found"
```

I've been messing around with Lisp lately and have wondered about the Lisp way
of doing something similar.

Because Lisp doesn't have (to my knowledge) a similar block syntax, we
can do something similar by passing in an anonymous function. Doing
setup, calling the function and then doing cleanup.

```lisp
(require :usocket) ; You may need to (ql:quickload :usocket)

(defun with_open_socket (addr port f)
  (let* ((socket (usocket:socket-connect addr port))
         (stream (usocket:socket-stream socket))
         (result (funcall f stream)))
    (usocket:socket-close socket))
    result)
```

Using our `with_open_socket` turns out to be very similar to the ruby
version except that we need to wrap our code in a lambda. Writing a
get request to a socket is much cleaner in Ruby but let's ignore that
today.

```lisp
(with_open_socket "www.google.com" 80
                  (lambda (stream)
                    (format stream "GET / HTTP/1.1~C~C~C~C"
                            #\Return #\Newline #\Return #\Newline)
                    (force-output stream)
                    (read-line stream)))

=> "HTTP/1.1 302 Found"
```

Now I'd like to see if we can forget the lambda and add a similar
block syntax to Lisp using a macro.

```lisp
(defmacro with-open-socket ((addr port) &body body)
  `(let* ((socket (usocket:socket-connect ,addr ,port))
          (stream (usocket:socket-stream socket))
          (result (progn ,@body)))
     (usocket:socket-close socket)
     result))
```

At read / compile time, `addr` and `port` arguments replace `,addr` and
`,port`. The body argument is pasted into the let statement.

We can call this macro similarly to above except that we no longer
need to pass in a lambda.

```lisp
(with-open-socket ("www.google.com" 80)
  (format stream "GET / HTTP/1.1~C~C~C~C"
          #\Return #\Newline #\Return #\Newline)
  (force-output stream)
  (read-line stream))

=> "HTTP/1.1 302 Found"
```

At read / compile time, the above macro call is expanded into the following.

```lisp
(let* ((socket (usocket:socket-connect "www.google.com" 80))
       (stream (usocket:socket-stream socket))
       (result (progn
                 (format stream "GET / HTTP/1.1~C~C~C~C"
                         #\Return #\Newline #\Return #\Newline)
                 (force-output stream)
                 (read-line stream))))
  (usocket:socket-close socket)
  result)
```

`with-open-socket` is effectively new syntax to the language that ends
up just as clean as the Ruby block syntax.

An interesting tidbit is that emacs knows that the the macro `with-open-socket`
has a `body` argument which means it's auto formatted with a two space indent
rather than formatting the body like a normal function call argument.
