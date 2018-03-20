# Introduction #
This package contains extensions for scala. These are designed to simplify the output of collections to text files,
provide statistical tools or implementations of data structures. The package structure is the following:
```
myscala
+-- math
|   +-- algo
|   +-- linalg
|   +-- stats
|   +-- vector
+-- output
+-- input
```
This package is managed with sbt, hence to compile and use the package the following steps need to be followed:
1. download the source code
2. run ```sbt``` in the root of the repository
3. inside sbt, run ```publishLocal``` to publish this package to your local machine

If all these steps completed without issue, this package can now be included in any other scala project by adding the
 following dependency in the ```build.sbt``` file.
 ```sbtshell
  "transpor.molyneaux" %% "scala-custom" % "1.0-SNAPSHOT",

```
Below are some examples of classic imports:
```scala
import myscala.timeBlock
import myscala.math.stats.{ComputeStats, stats}
import myscala.output.SeqOfSeqExtensions.SeqOfSeqWriter
import myscala.output.SeqTuplesExtensions.SeqTuplesWriter
```