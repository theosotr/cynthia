/*
 * Copyright (c) 2020-2021 Thodoris Sotiropoulos, Stefanos Chaliasos
 *
 * This program is free software: you can redistribute it and/or modify  
 * it under the terms of the GNU General Public License as published by  
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License 
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cynthia.utils

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
