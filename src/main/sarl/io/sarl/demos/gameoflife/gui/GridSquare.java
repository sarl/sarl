package io.sarl.demos.gameoflife.gui;

import javafx.scene.Parent;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;

/**
 * The type GridSquare.
 *
 * @author Maxime PINARD
 */
public class GridSquare extends Parent {

	private static final int SQUARE_SIZE = 16;
	private static final Color ON_COLOR = Color.RED;
	private static final Color OFF_COLOR = Color.BLACK;

	private final Rectangle rectangle;
	private final int xPosition;
	private final int yPosition;
	private boolean activated;

	/**
	 * Instantiates a new GridSquare.
	 *
	 * @param xPosition the x position
	 * @param yPosition the y position
	 * @param activated the activated
	 */
	public GridSquare(int xPosition, int yPosition, boolean activated) {
		this.xPosition = xPosition;
		this.yPosition = yPosition;
		this.activated = activated;
		this.rectangle = new Rectangle(this.xPosition * SQUARE_SIZE, this.yPosition * SQUARE_SIZE, SQUARE_SIZE, SQUARE_SIZE);
		this.rectangle.setFill(this.activated ? ON_COLOR : OFF_COLOR);
		getChildren().add(this.rectangle);
	}

	/**
	 * Gets square size.
	 *
	 * @return the square size
	 */
	public static int getSquareSize() {
		return SQUARE_SIZE;
	}

	/**
	 * Sets the square activated or not.
	 * Color of the square depend of is activation.
	 *
	 * @param activated the activated
	 */
	public void setActivated(boolean activated) {

		if(activated != this.activated) {
			this.activated = activated;
			this.rectangle.setFill(this.activated ? ON_COLOR : OFF_COLOR);
		}
	}

}

