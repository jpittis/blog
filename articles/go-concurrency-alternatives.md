# Go concurrency alternatives: Haskell's race!

**Jan 16, 2018**

I'm tired of Go concurrency boilerplate and silly Ruby bugs that could be
caught by a static type system. This recently brought me to Haskell. I came for
the fp, and the implicit static type system. And now it looks like I also want
to stay for the awesome concurrency.

## Haskell's race function.

It's not uncommon to want to run two IO operations concurrently and use the
result of the operation that completes first.

This is what Haskell's async package's `race` function accomplishes.

```haskell
race :: IO a -> IO b -> IO (Either a b)
```

You give it two "things to do" and it will run them concurrently. It will return
the result of whichever completes first, and cancel the other one.

```haskell
ghci> race ((threadDelay $ 1^6) >> return 1) (return 2)
2
```

The first operation will pause for 1 second (1,000,000 micro seconds) and the
second operation will return immediately. Because the `return 2` finishes
before the `threadDelay`, the `return 1` is canceled and never completes.

(For those not familiar with the `$` operation, that's equivilant to using
brackets like `(threadDelay (1^6))`. For those not familiar with the `>>`
operation, that's saying do the thing on the left and then the thing on the
right. If you come from a Go or Ruby world, `return` doesn't mean what you
think it means... I won't try to explain it.)

## A naive implementation in Go.

Here's a naive Go implementation of `race`.

```go
type ResultFunction func() interface{}

func race(f1, f2 ResultFunction) interface{} {
	result := make(chan interface{}, 2)

	go func() { result <- f2() }()
	go func() { result <- f1() }()

	return <-result
}
```

Here's are a couple fake `ResultFunction`s to test the implementation.

```go
func quickly() interface{} {
	time.Sleep(1 * time.Second)
	return "quickly!"
}

func slowly() interface{} {
	time.Sleep(3 * time.Second)
	return "slowly!"
}
```

And we can see it always returns the faster of the two `ResultFunctions`.

```go
race(quickly, slowly) == "quickly!"
race(slowly, quickly) == "quickly!"
```

The implementations inadequacy is exposed when you test it with a blocking
`ResultFunction`.

```go
func blocking() interface{} {
	neverReturn := make(chan interface{})
	return <-neverReturn
}
```

At first it seems to behave correctly.

```go
race(blocking, quickly) == "quickly!"
```

But if you think about it, we've just leaked a Goroutine.

Go has no way to cancel a Goroutine from the outside. To cancel our
`ResultFunction`s, we're going to have to design them specificly to be
cancelable.

## Using the context package.

The Go community has standardised around using the `context` package to cancel
Goroutines.

We can rewrite our `ResultFunction` to take a cancelable `context.Context`.
Let's add error handling while we're at it.

```go
type ResultFunction func(context.Context) (interface{}, error)
```

This means all our `ResultFunction`s need to be written in a non-blocking
`context` package style.

```go
func quickly(ctx context.Context) (interface{}, error) {
	select {
	case <-time.After(1 * time.Second):
		return "quickly!", nil
	case <-ctx.Done():
		return nil, ctx.Err()
	}
}

func slowly(ctx context.Context) (interface{}, error) {
	select {
	case <-time.After(3 * time.Second):
		return "slowly!", nil
	case <-ctx.Done():
		return nil, ctx.Err()
	}
}

func blocking(ctx context.Context) (interface{}, error) {
	select {
	case <-ctx.Done():
		return nil, ctx.Err()
	}
}
```

And here's the new implementation of `race`.

```go
func race(ctx context.Context, f1, f2 ResultFunction) (interface{}, error) {
	result := make(chan interface{}, 2)
	fail := make(chan error, 2)

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	go func() {
		r, err := f1(ctx)
		if err != nil {
			fail <- err
		} else {
			result <- r
		}
	}()

	go func() {
		r, err := f2(ctx)
		if err != nil {
			fail <- err
		} else {
			result <- r
		}
	}()

	select {
	case r := <-result:
		return r, nil

	case err := <-fail:
		return nil, err

	case <-ctx.Done():
		return nil, ctx.Err()
	}
}
```

Let's test out with are old test cases.

```go
race(context.Background(), quickly, slowly) == ("quickly!", nil)
race(context.Background(), blocking, quickly) == ("quickly!", nil)
```

And this time, the blocking function returns.

We can also test top level cancelation.

```go
ctx, cancel := context.WithCancel(context.Background())
go func() {
  _, err := race(ctx, quickly, slowly) // err != nil
}()
cancel()
```

And we can test a function that fails, causes the other one to cancel.


```go
_, err := race(context.Background(), erroring, blocking) // err == "Boom!"
```


## The pain points.

Now that we've implemented `race` in Go, let's discuss it's issues.

### `context` boilerplate at the call site.

Though I find that always passing around context values to be annoying, this is
an effective way to encode a sort of cancelation tree into your program.

### Non blocking `ResultFunctions`.

This is a huge pain point. I need to design all the functions I want to run
concurrently such that they respond to `cancel` and never block. This means
lots of `select` boilerplate. Because I'm writing the boilerplate by hand,
there's always room for bugs. And I can't use other people's blocking code
because it will leak Goroutines.

### Type `interface{}`.

The Go community is probably tired of hearing it, but fuck am I tired of not
having generics.

## Haskell's solution.

So far, my experience with Haskell's `async` package has been great. It
relieves the annoyances of Go quite well. I'm going to keep on experimenting
with it and report back with more findings.
