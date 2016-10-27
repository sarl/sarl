package io.sarl.demos.gameoflife.gui;

/**
 * The interface ControllerListener.
 *
 * @author Maxime PINARD
 */
public interface ControllerListener {

	/**
	 * Ready to setup state setter.
	 *
	 * @param state the state
	 */
	void setReadyToSetup(boolean state);

	/**
	 * Ready to play state setter.
	 *
	 * @param state the state
	 */
	void setReadyToPlay(boolean state);

	/**
	 * Ready to pause state setter.
	 *
	 * @param state the state
	 */
	void setReadyToPause(boolean state);
}
