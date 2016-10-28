package io.sarl.demos.gameoflife.gui;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.Slider;
import javafx.scene.control.TextField;
import javafx.scene.layout.Pane;

/**
 * The type SimulationViewController.
 *
 * @author Maxime PINARD
 */
public class SimulationViewController {
	@FXML
	public TextField widthTextField;
	@FXML
	public TextField heightTextField;
	@FXML
	public Button setupButton;
	@FXML
	public ProgressBar setupProgressBar;
	@FXML
	public Label timeIntervalLabel;
	@FXML
	public Slider timeIntervalSlider;
	@FXML
	public Button timeIntervalButton;
	@FXML
	public Button playButton;
	@FXML
	public Button pauseButton;
	@FXML
	public Button exitButton;
	@FXML
	public Pane simulationPane;

	public SimulationViewController() {
	}

	@FXML
	private void initialyse() {
	}
}
