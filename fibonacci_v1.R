#Fibonacci

fibonacci = function(n)
{
  if (n < 0)
  {
    "no valido"
  }
  else if (n <= 1)
  {
    n
  }
  else
  {
    fibonacci(n - 2) + fibonacci(n - 1)
  }
}

fibonacci(2)



