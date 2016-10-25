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

		this.gridWidth = gridWidth;
		this.grisheight = gridheight;

		this.gridSquares = new GridSquare[this.gridWidth][this.grisheight];
		GridSquare[][] gridSquares1 = this.gridSquares;
		for(int i = 0; i < gridSquares1.length; i++) {
			GridSquare[] squares = gridSquares1[i];
			for(int j = 0; j < squares.length; j++) {
				GridSquare square = squares[j];
				square = new GridSquare(i, j, false);
				getChildren().add(square);
			}
		}
	}
}
