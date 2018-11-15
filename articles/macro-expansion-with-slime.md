# Macro Expansion in your Editor

**Aug 07, 2017**

I've been blown away by the tooling that Emacs provides for editing Lisp. I've
been using the `evil-mode` package to simulate `vim` key bindings and the
`slime` package for "superior" Lisp editing.

Running the `slime` function opens a Lisp REPL on the right side of my editor.
I've bound `,sc` to compile the current file I'm editing and send it to the
REPL so I can play around with it. `,se` automaticly evalutes the expression
under my cursor and sends the result to a buffer. This has greatly sped up my
feedback loop for knowing if my code works.

I've been playing a lot with Lisp macros lately and my editor has surprised me
yet again!

Macros are like functions that manipulate your programs AST at compile time to
produce code that will run at runtime.

The macro I want to share with you today is for generating code that converts a
buffer of bytes into an integer.

```lisp
(defmacro read-int-from-bytes (buffer offset size)
  `(let ((int 0))
     (declare ((signed-byte ,(* 8 size)) int))
     ,@(loop for n from 0 to (- size 1)
          collect `(setf (ldb (byte 8 (* 8 ,n)) int) (aref ,buffer (+ ,offset ,n))))
     int))
```

Oh my gosh that looks complicated!? I wonder what code it expands into when we
call it.

All I need to do is type `(read-int-from-bytes #(1) 0 1)` into my editor.
Hitting `,sm` calls `macroexpand-1` and sends the following code into my right
buffer.

```lisp
(LET ((INT 0))
  (DECLARE ((SIGNED-BYTE 8) INT))
  (SETF (LDB (BYTE 8 (* 8 0)) INT) (AREF #(1) (+ 0 0)))
  INT)
```

This code looks a little simpler to understand. We first create an integer
called `INT` and set it to zero. We declare this integer will be of size 8
bits. And then we set the first and only byte of the integer to be the first
byte found in the buffer!

This code basicly read one byte from a given buffer. Which could be done much
more simply. But what if we want to read in a 32 bit integer rather than an 8
bit integer?

Let's type `(read-int-from-bytes #(0 1 0 0) 4)` into our editor and run
`macroexpand-1` again.

```lisp
(LET ((INT 0))
  (DECLARE ((SIGNED-BYTE 32) INT))
  (SETF (LDB (BYTE 8 (* 8 0)) INT) (AREF #(0 1 0 0) (+ 0 0)))
  (SETF (LDB (BYTE 8 (* 8 1)) INT) (AREF #(0 1 0 0) (+ 0 1)))
  (SETF (LDB (BYTE 8 (* 8 2)) INT) (AREF #(0 1 0 0) (+ 0 2)))
  (SETF (LDB (BYTE 8 (* 8 3)) INT) (AREF #(0 1 0 0) (+ 0 3)))
  INT)
```

This shows the benefit of our macro. It will generate code that converts an
arbitrary number of bytes from an arbitrary offset in a buffer into a single
integer.

Of course... this is pretty cool. I can see what the macros I write expand into
right from my editor. What's even cooler is that I can now evaluate this
expanded code.

I switch my cursor to the buffer that shows the expanded macro above. And then
I hit `,se` to evaluate the expression.

```lisp
=> 256
```

This shows up in the minibuffer at the bottom of my editor.

I'm sure this macro is terribly written because I have no idea what I'm doing!
But who cares... I haven't had this much fun programming in as long as I can
remember!
