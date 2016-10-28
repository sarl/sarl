package io.sarl.demos.gameoflife.gui;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
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

	private final int minValue = 0;
	private final int maxValue = 100;

	public SimulationViewController() {
	}

	@FXML
	private void initialyse() {
		this.widthTextField.textProperty().addListener(new ChangeListener<String>() {
			@Override
			public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
				if(!newValue.equals("")) {
					if(newValue.matches("[0-9]*") && validateValue(Integer.parseInt(newValue))) {
						SimulationViewController.this.widthTextField.setText(newValue);
					}
					else {
						SimulationViewController.this.widthTextField.setText(oldValue);
					}
				}
				else {
					SimulationViewController.this.widthTextField.setText(newValue);
				}
			}
		});
		this.heightTextField.textProperty().addListener(new ChangeListener<String>() {
			@Override
			public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
				if(!newValue.equals("")) {
					if(newValue.matches("[0-9]*") && validateValue(Integer.parseInt(newValue))) {
						SimulationViewController.this.heightTextField.setText(newValue);
					}
					else {
						SimulationViewController.this.heightTextField.setText(oldValue);
					}
				}
				else {
					SimulationViewController.this.heightTextField.setText(newValue);
				}
			}
		});
	}

	private boolean validateValue(int value) {
		return (value >= this.minValue && value <= this.maxValue);
	}
}
