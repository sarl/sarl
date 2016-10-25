package io.sarl.demos.gameoflife.gui;

import javafx.beans.property.DoubleProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.event.EventHandler;
import javafx.scene.Parent;
import javafx.scene.input.MouseEvent;
import javafx.scene.input.ScrollEvent;

/**
 * The type DraggableZoomableParent.
 *
 * @author Maxime PINARD
 */
public abstract class DraggableZoomableParent extends Parent {

	private static final double MAX_SCALE = 10.0d;
	private static final double MIN_SCALE = .1d;
	private static final double DELTA = 1.1d;
	private final DoubleProperty scale = new SimpleDoubleProperty(1.0);

	private double leftMouseAnchorX;
	private double leftMouseAnchorY;
	private double translateAnchorX;
	private double translateAnchorY;

	private void setPivot(double x, double y) {
		setTranslateX(getTranslateX() - x);
		setTranslateY(getTranslateY() - y);
	}

	private double clamp(double value, double min, double max) {

		if(Double.compare(value, min) < 0)
			return min;

		if(Double.compare(value, max) > 0)
			return max;

		return value;
	}

	/**
	 * Instantiates a new DraggableZoomableParent.
	 */
	public DraggableZoomableParent() {

		scaleXProperty().bind(this.scale);
		scaleYProperty().bind(this.scale);

		setOnMousePressed(new EventHandler<MouseEvent>() {

			@Override
			public void handle(MouseEvent event) {

				// left mouse button
				if(event.isPrimaryButtonDown()) {

					DraggableZoomableParent.this.leftMouseAnchorX = event.getSceneX();
					DraggableZoomableParent.this.leftMouseAnchorY = event.getSceneY();

					DraggableZoomableParent.this.translateAnchorX = getTranslateX();
					DraggableZoomableParent.this.translateAnchorY = getTranslateY();

					event.consume();
				}
			}
		});

		setOnMouseDragged(new EventHandler<MouseEvent>() {

			@Override
			public void handle(MouseEvent event) {

				// right mouse button
				if(event.isPrimaryButtonDown()) {

					DraggableZoomableParent.this.setTranslateX(DraggableZoomableParent.this.translateAnchorX + event.getSceneX() - DraggableZoomableParent.this.leftMouseAnchorX);
					DraggableZoomableParent.this.setTranslateY(DraggableZoomableParent.this.translateAnchorY + event.getSceneY() - DraggableZoomableParent.this.leftMouseAnchorY);

					event.consume();
				}
			}
		});

		setOnScroll(new EventHandler<ScrollEvent>() {
			@Override
			public void handle(ScrollEvent event) {

				double scale = DraggableZoomableParent.this.scale.get();
				double oldScale = scale;

				if(event.getDeltaY() < 0)
					scale /= DELTA;
				else
					scale *= DELTA;

				scale = clamp(scale, MIN_SCALE, MAX_SCALE);

				double f = (scale / oldScale) - 1;

				double dx = (event.getSceneX() - (DraggableZoomableParent.this.getBoundsInParent().getWidth() / 2 + DraggableZoomableParent.this.getBoundsInParent().getMinX()));
				double dy = (event.getSceneY() - (DraggableZoomableParent.this.getBoundsInParent().getHeight() / 2 + DraggableZoomableParent.this.getBoundsInParent().getMinY()));

				DraggableZoomableParent.this.scale.set(scale);

				// note: pivot value must be untransformed, i. e. without scaling
				DraggableZoomableParent.this.setPivot(f * dx, f * dy);

				event.consume();
			}
		});
	}
}
