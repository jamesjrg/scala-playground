import scala.io.Source

def fileToArray(filename: String) = {
    val bufferedSource = Source.fromFile(filename)
    val arr = bufferedSource.getLines.toArray
    bufferedSource.close
    arr
}


