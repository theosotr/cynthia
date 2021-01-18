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

package cynthia

import cynthia.utils.RUtils

object Cynthia {

  def main(args: Array[String]): Unit = {
    OptionParser.cliParser.parse(args, Options()) map (options => {
      RUtils.seed(options.randomSeed)
      Controller(options)
    }) getOrElse {
      // Wrong arguments
    }
  }
}
