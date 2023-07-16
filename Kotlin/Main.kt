import java.io.File
import java.io.IOException
import java.util.*

fun main() {
    val filename = "day_1_input.txt"
    val topThree: MutableList<Int> = mutableListOf(0, 0, 0)
    var currentDwarf = 0

    try {
        val file = File(filename)
        val sc = Scanner(file)
        while (sc.hasNextLine()) {
            val line = sc.nextLine()
            if (line == "") {
                if (topThree[0] < currentDwarf) {
                    topThree[0] = currentDwarf
                    topThree.sort()
                }

                currentDwarf = 0
            } else {
                currentDwarf += line.toInt()
            }
        }
    } catch (e: IOException) {
        e.printStackTrace()
    }

    print(topThree)
    print(topThree.sum())
}