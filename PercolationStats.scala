////Andrew Escobedo
//introduction to algorithms

import edu.princeton.cs.algs4.{StdRandom, StdStats}

class PercolationStats(val n:Int, val trials:Int) {

    private val _sites = new Array[Double](trials)
    for(t <- 0 until trials){ runSimulation(t) }

    def mean(): Double = StdStats.mean(_sites) / (n * n)

    def stddev(): Double = (StdStats.stddev(_sites)) / (n * n)

    def confidenceLo(): Double = mean() - ((1.96 * stddev())) / Math.sqrt(trials-1)

    def confidenceHi(): Double = mean() + ((1.96 * stddev())) / Math.sqrt(trials-1)

    private def runSimulation(t:Int): Unit = {
        val percolate = new Percolation(n)
        var num_sites_open = 0
        while (!percolate.percolates()) {
            val row = StdRandom.uniform(1,n+1)
            val col = StdRandom.uniform(1,n+1)

            if (!percolate.isOpen(row,col)) {
                percolate.open(row, col)
                num_sites_open += 1
            }
        }
        _sites(t) = num_sites_open
    }
}


object Main{
    def main(args: Array[String]): Unit = {
        val (n, trials) = (args(0).toInt,args(1).toInt)
        if (n < 0 || trials < 0) throw new IllegalArgumentException("n or trials <= 0")
        if (args.length != 2) throw new IllegalArgumentException("args != 2")
        val percstats = new PercolationStats(n,trials)
        println(s"n = $n, trials = $trials")
        println(s"mean = ${percstats.mean()}")
        println(s"stddev = ${percstats.stddev()}")
        println(s"95% confidence interval = ${percstats.confidenceLo()}, ${percstats.confidenceHi}")
    }
}
