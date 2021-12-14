#Fibonacci

fibonacci = function(n)
{
  serieF = (0)
  vectorN = (0:n)
  cnt = 0

  for (i in vectorN)
  {
    cnt = cnt + 1
    if (i < 0)
    {
      print("no valido")
    }
    else if(i <= 1)
    {
      serieF = append(serieF, i)
    }
    else
    {
      serieF = append(serieF, vectorN[cnt-1])
    }
  }
  print(serieF)
  print(vectorN)
  
}


fibonacci(5)



