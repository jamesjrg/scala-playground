import scala.io.Source

def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }

def fileToArray(filename: String) = {
    using (Source.fromFile(filename)) {
      bufferedSource =>
        bufferedSource.getLines.toArray
    }
}


