package io.sarl.demos.gameoflife.gui;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.TextField;

/**
 * The type RestrictedNumberTextField.
 *
 * @author Maxime PINARD
 */
public class RestrictedNumberTextField extends TextField {

	private final int minValue;
	private final int maxValue;

	public RestrictedNumberTextField(int defaultValue, int minValue, int maxValue) {
		super(Integer.toString(defaultValue));
		this.minValue = minValue;
		this.maxValue = maxValue;
		textProperty().addListener(new ChangeListener<String>() {
			@Override
			public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
				if(!newValue.equals("")){
					if(newValue.matches("[0-9]*") && validateValue(Integer.parseInt(newValue))) {
						setText(newValue);
					}
					else {
						setText(oldValue);
					}
				}
				else{
					setText(newValue);
				}
			}
		});
	}

	private boolean validateValue(int value) {
		return (value >= this.minValue && value <= this.maxValue);
	}
}
