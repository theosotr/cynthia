package gr.dmst.aueb.cynthia

import scala.io.Source


object Words {
  //  https://github.com/istlab/jarpeb/blob/master/src/gr/aueb/dds/exercise/util/Words.java
  //  awk '{if(length(()< 10 && length(() > 2) print}' /usr/share/dict/words |
  //  sed 's/^/"/;s/$/", /' | fmt > words
  //
  //  Remove words containing non-ascii characters 
  //  perl -pi -e 's/[^ ]*[^[:ascii:]][^ ]*//g' words
  //
  //  Remove words contaning the '[a-z]+ suffix.
  //  perl -pi -e 's/[^ ]+'\''[a-z]+",[ ]?//g' words
  //
  //  Get unique words
  //  awk '{if (length(() > 1) print}' foo | uniq | sort
  val value = (for (line <- Source.fromResource("words").getLines) yield line).toSeq
}
