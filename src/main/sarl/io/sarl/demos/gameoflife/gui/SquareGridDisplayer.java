package io.sarl.demos.gameoflife.gui;

/**
 * The type SquareGridDisplayer.
 *
 * @author Maxime PINARD
 */
public class SquareGridDisplayer extends DraggableZoomableParent {

	private int gridWidth;
	private int grisheight;
	private GridSquare gridSquares[][];

	/**
	 * Instantiates a new SquareGridDisplayer.
	 *
	 * @param gridWidth  the grid width
	 * @param gridheight the grid height
	 */
	public SquareGridDisplayer(int gridWidth, int gridheight) {

		if(gridWidth == 0 || gridheight == 0) {
			throw new IllegalArgumentException("gridWidth or gridHeight is equal to 0");
		}

		this.gridWidth = gridWidth;
		this.grisheight = gridheight;

		this.gridSquares = new GridSquare[this.gridWidth][this.grisheight];
		for(int i = 0; i < this.gridSquares.length; ++i) {
			for(int j = 0; j < this.gridSquares[i].length; ++œj) {
				this.gridSquares[i][j] = new GridSquare(i, j, false);
				getChildren().add(this.gridSquares[i][j]);
			}
		}
	}

	/**
	 * Sets the grid.
	 *
	 * @param grid the grid
	 */
	void setGrid(boolean[][] grid) {
		if(grid.length == this.gridSquares.length && grid[0].length == this.gridSquares[0].length) {
			for(int i = 0; i < this.gridSquares.length; i++) {
				for(int j = 0; j < this.gridSquares[i].length; j++) {
					this.gridSquares[i][j].setActivated(grid[i][j]);
				}
			}
		}
	}
}
