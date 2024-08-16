/*
 * Copyright (C) 2022  Xoán C. Pardo
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package cec2013.util

import cec2013.util.SearchSpace.Element

import java.io.{Closeable, File, FileInputStream, InputStream}
import scala.io.{BufferedSource, Source}
import scala.reflect.ClassTag

/**
 * Definitions to read cdatafiles
 */
object DataFileReader {

  /**
   * Parse a line (a string)
   *
   * Parsers for the types found in cdatafiles (Int, Double, Vector) are defined
   */
  private type LineParser[T] = String => T

  private object LineParser {
    def apply[T](f: String => T): LineParser[T] = f(_)

    def apply[T](f: String => T, sep: String)(implicit c: ClassTag[T]): LineParser[Vector[T]] = _.split(sep).map(f(_)).toVector

    def asInt(): LineParser[Int] = LineParser({ s: String => s.toInt })

    def asDouble(): LineParser[Double] = LineParser({ s: String => s.toDouble })

    def asVectorOfInts(): LineParser[Vector[Int]] = LineParser({ s: String => s.toInt }, ",")

    def asVectorOfDoubles(): LineParser[Vector[Double]] = LineParser({ s: String => s.toDouble }, ",")
  }

  /**
   * Parse a file (a BufferedSource) line by line using a given LineParser
   */
  private type FileParser[T] = BufferedSource => Iterator[T]

  private object FileParser {
    def apply[T](parser: LineParser[T]): FileParser[T] = source => source.getLines().map(parser)
  }

  // auxiliary type definitions for parsing cdatafiles values
  type Permutations = Vector[Int]
  type RotationMatrix = Vector[Vector[Double]]
  type Subcomponents = Vector[Int]
  type Weights = Vector[Double]

  // implicit class to add parsing methods to BufferedSource
  private implicit class FileParserOps(val source: BufferedSource) {
    def asElement(): Element = FileParser(LineParser.asDouble())(source).toVector

    def asPermutations(): Permutations = FileParser(LineParser.asVectorOfInts())(source).next

    def asRotationMatrix(): RotationMatrix = FileParser(LineParser.asVectorOfDoubles())(source).toVector

    def asSubcomponents(): Subcomponents = FileParser(LineParser.asInt())(source).toVector

    def asWeights(): Weights = asElement() // same format is used in both files
  }

  /** implementation of the Loan pattern
   *
   * it closes a resource (file) in case of failure
   */
  private object LoanPattern {
    private[DataFileReader] def using[A <: Closeable, B](resource: A)(f: A => B): B =
      try {
        f(resource)
      } finally {
        resource.close()
      }
  }

  /**
   * Trait to add operations to read the cdatafiles packaged as resources
   */
  trait CDataFileOps {

    import LoanPattern._

    // path to cdatafiles (packaged as resources)
    private val cdatapath: String =
//      "cec2013/cdatafiles/"
//      "src/main/resources/cec2013/cdatafiles/"
          "C:\\Users\\LENOVO\\Desktop\\Learn\\Personal Project\\PDCCSO\\src\\main\\resources\\cec2013\\cdatafiles\\"

    /** reads a cdatafile as a resource using a given parser
     *
     * @param name   the name of the cdatafile (the path and extension are automatically prepended and appended)
     * @param parser the file parser
     * @tparam T the type of the read data
     * @return the values read from the cdatafile
     * @throws IllegalArgumentException if the resource could not be found
     */
    private def read[T](name: String)(parser: BufferedSource => T): T = {
      val rscPath = cdatapath + name + ".txt"
      val file = new File(rscPath)
      val inputStream: InputStream = new FileInputStream(file)
      Option(inputStream) match {
        case Some(rsc) => using(Source.fromInputStream(rsc)) {
          parser(_)
        }
        case None => throw new IllegalArgumentException("resource not found: " + rscPath)
      }
    }

    /**
     * Functions to read the cdatafiles of a given benchmark function
     *
     * @param id the id of the benchmark function
     */
    def readOptimal(id: String): Element = read(id + "-xopt") {
      _.asElement()
    }

    def readPermutations(id: String): Permutations = read(id + "-p") {
      _.asPermutations()
    }

    def readRotation(id: String, size: Int): RotationMatrix = read(id + "-R" + size) {
      _.asRotationMatrix()
    }

    def readSubcomponents(id: String): Subcomponents = read(id + "-s") {
      _.asSubcomponents()
    }

    def readWeights(id: String): Weights = read(id + "-w") {
      _.asWeights()
    }
  }

  /**
   * Trait to add an operation to read an Element from file
   */
  trait ElementReader {

    import LoanPattern._

    /** reads an Element from a given file
     *
     * @param name the full path of the file
     * @return the element read from the file
     * @throws FileNotFoundException if the file could not be found
     */
    def readElement(name: String): Element =
      using(Source.fromFile(name)) {
        _.asElement()
      }
  }

}
