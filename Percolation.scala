//Andrew Escobedo
//introduction to algorithms

import edu.princeton.cs.algs4.WeightedQuickUnionUF

class Percolation(val n:Int) {

    private val (_size, _top, _bottom, _dim) = (n*n+2, 0, n*n+1, n)
    private val _sites = Array.fill[Boolean](_size)(false)
    private val _uf = new WeightedQuickUnionUF(_size)
    _sites(_top) = true
    _sites(_bottom) = true

    def open(row:Int, col:Int): Unit = {
        validate(row,col)
        if(isOpen(row,col)) return              //returns if site is already open

        val (up, down, left, right, index) = (row-1, row+1, col-1, col+1, indexOf(row,col))

        if(isOpen(up,col)) _uf.union(indexOf(up,col), index)
        if(isOpen(down,col)) _uf.union(indexOf(down,col), index)
        if(isOpen(row,left)) _uf.union(indexOf(row,left), index)
        if(isOpen(row,right)) _uf.union(indexOf(row,right), index)

        _sites(index) = true
    }

    def isOpen(row:Int, col:Int): Boolean = _sites(indexOf(row,col))

    def isFull(row:Int, col:Int): Boolean = _uf.connected(indexOf(row,col),_top)

    def percolates(): Boolean = _uf.connected(_top,_bottom)


    private def validate(row:Int, col:Int): Unit = {
        if (row < 1 || row > _dim || col < 1 || col > _dim)
            throw new ArrayIndexOutOfBoundsException(s"row ${row} and col ${col} must be between 1 and ${_dim}")
    }

    private def indexOf(row:Int, col:Int): Int = {
        var c = col
        if(row < 1) return _top             //------ row 0 every elmnt is _top
        if(row > _dim) return _bottom       //000000 row 1 every elmnt connected to _top
        if(col < 1)    c = 1                //000000 row 2
        if(col > _dim) c = _dim             //...... row n
                                            //000000 row (_dim) every elmnt connected to _bottom
        ((row-1) * _dim) + c                //------ row (_dim+1) every elmnt is _bottom
    }
}

