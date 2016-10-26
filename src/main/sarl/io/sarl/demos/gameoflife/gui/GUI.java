package io.sarl.demos.gameoflife.gui;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.eclipse.xtext.xbase.lib.Pair;

import io.sarl.demos.gameoflife.environment.agent.EnvironmentListener;
import javafx.application.Application;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

/**
 * The type GUI.
 *
 * @author Maxime PINARD
 */
public class GUI extends Application implements EnvironmentListener {

	private static GUI gui;
	private final List<GUIListener> listeners = new ArrayList<>();
	private SquareGridDisplayer squareGridDisplayer;
	private Stage primaryStage;

	/**
	 * Gets gui.
	 *
	 * @return the gui
	 */
	public static GUI getGUI() {
		if(gui != null) {
			launch();
		}
		return gui;
	}

	/**
	 * Launch the gui.
	 *
	 * @param width  the width
	 * @param height the height
	 */
	public void launchGUI(int width, int height) {
		this.primaryStage.setOnCloseRequest(new EventHandler<WindowEvent>() {
			@Override
			public void handle(WindowEvent event) {
				for(GUIListener guiListener : GUI.this.listeners) {
					guiListener.stop();
				}
			}
		});
		this.primaryStage.setTitle("Sarl game of life demo");

		this.squareGridDisplayer = new SquareGridDisplayer(width, height);
		Scene scene = new Scene(this.squareGridDisplayer);

		this.primaryStage.setScene(scene);
		this.primaryStage.show();
	}

	@Override
	public void start(Stage primaryStage) {
		gui = this;
		this.primaryStage = primaryStage;
	}

	/**
	 * Add a gui listener.
	 *
	 * @param listener the listener
	 */
	public void addGUIListener(GUIListener listener) {

		this.listeners.add(listener);
	}

	@Override
	public void handleGridUpdate(List<List<Pair<UUID, Boolean>>> grid) {

		if(grid.size() == 0 || grid.get(0).size() == 0) {
			throw new IllegalArgumentException("grid width or grid height is equal to 0");
		}

		boolean[][] booleenGrid = new boolean[grid.size()][grid.get(0).size()];
		for(int i = 0; i < grid.size(); ++i) {
			for(int j = 0; j < grid.get(i).size(); ++j) {
				booleenGrid[i][j] = grid.get(i).get(j).getValue();
			}
		}

		this.squareGridDisplayer.setGrid(booleenGrid);
	}
}
