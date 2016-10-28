package io.sarl.demos.gameoflife.gui;

import io.sarl.demos.gameoflife.environment.agent.EnvironmentListener;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;
import org.eclipse.xtext.xbase.lib.Pair;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * The type GUI.
 *
 * @author Maxime PINARD
 */
public class GUI extends Application implements EnvironmentListener, ControllerListener {

	private static GUI gui;
	private final List<GUIListener> listeners = new ArrayList<>();
	private Stage primaryStage;
	private SimulationViewController controller;
	private SquareGridDisplayer squareGridDisplayer;
	private boolean inited = false;
	private int gridWidth;
	private int gridHeight;
	private SimpleIntegerProperty timeInterval = new SimpleIntegerProperty();
	private SimpleBooleanProperty readyToPlay = new SimpleBooleanProperty(false);
	private SimpleBooleanProperty readyToPause = new SimpleBooleanProperty(false);
	private SimpleBooleanProperty readyToSetup = new SimpleBooleanProperty(false);

	/**
	 * Gets gui.
	 *
	 * @return the gui
	 */
	public static GUI getGUI() {
		if(gui == null) {
			ExecutorService executorService = Executors.newSingleThreadExecutor();
			executorService.submit((Runnable) Application::launch);

			while(gui == null) {
				Thread.yield();
			}
		}

		return gui;
	}

	/**
	 * Launch the gui.
	 */
	public void launchGUI() {

		Platform.runLater(this::initGUI);
	}

	private void initGUI() {

		FXMLLoader loader = new FXMLLoader();
		loader.setLocation(GUI.class.getResource("SimulationView.fxml"));
		HBox mainHBox = new HBox();
		try {
			mainHBox = (HBox) loader.load();
		} catch(IOException e) {
			e.printStackTrace();
		}
		this.controller = loader.getController();

		this.controller.widthTextField.setText("10");
		this.controller.heightTextField.setText("10");

		this.controller.setupButton.disableProperty().bind(Bindings.not(this.readyToSetup));
		this.controller.setupButton.setOnAction(new EventHandler<ActionEvent>() {
			@Override
			public void handle(ActionEvent event) {
				GUI.this.gridWidth = toInt(GUI.this.controller.widthTextField.getText());
				GUI.this.gridHeight = toInt(GUI.this.controller.heightTextField.getText());
				for(GUIListener listener : GUI.this.listeners) {
					listener.setup(GUI.this.gridWidth, GUI.this.gridHeight);
				}
			}
		});

		this.timeInterval.bind(this.controller.timeIntervalSlider.valueProperty());
		this.controller.timeIntervalLabel.textProperty().bind(this.timeInterval.asString());

		this.controller.timeIntervalSlider.setMin(0);
		this.controller.timeIntervalSlider.setMax(1000);
		this.controller.timeIntervalSlider.setBlockIncrement(1);
		this.controller.timeIntervalSlider.setValue(500);

		this.controller.timeIntervalButton.setOnAction(new EventHandler<ActionEvent>() {
			@Override
			public void handle(ActionEvent event) {
				for(GUIListener listener : GUI.this.listeners) {
					listener.periodUpdated(GUI.this.timeInterval.getValue());
				}
			}
		});

		this.controller.playButton.disableProperty().bind(Bindings.not(this.readyToPlay));
		this.controller.playButton.setOnAction(new EventHandler<ActionEvent>() {
			@Override
			public void handle(ActionEvent event) {
				GUI.this.listeners.forEach(GUIListener::play);
			}
		});

		this.controller.pauseButton.disableProperty().bind(Bindings.not(this.readyToPause));
		this.controller.pauseButton.setOnAction(new EventHandler<ActionEvent>() {
			@Override
			public void handle(ActionEvent event) {
				GUI.this.listeners.forEach(GUIListener::pause);
			}
		});

		this.controller.exitButton.setOnAction(new EventHandler<ActionEvent>() {
			@Override
			public void handle(ActionEvent event) {
				GUI.this.primaryStage.close();
				GUI.this.listeners.forEach(GUIListener::stop);
			}
		});

		this.primaryStage.setOnCloseRequest(new EventHandler<WindowEvent>() {
			@Override
			public void handle(WindowEvent event) {
				GUI.this.listeners.forEach(GUIListener::stop);
			}
		});

		Scene scene = new Scene(mainHBox);
		this.primaryStage.setTitle("Sarl game of life demo");
		this.primaryStage.setScene(scene);
		this.primaryStage.show();
		this.inited = true;
	}

	/**
	 * Setup the gui.
	 *
	 * @param width  the grid width
	 * @param height the grid height
	 */
	public void setupGUI(int width, int height) {
		if(this.inited) {
			this.gridWidth = width;
			this.gridHeight = height;
			this.squareGridDisplayer = new SquareGridDisplayer(this.gridWidth, this.gridHeight);
			Platform.runLater(() -> {
				this.controller.simulationPane.getChildren().clear();
				this.controller.simulationPane.getChildren().add(this.squareGridDisplayer);
			});
		}
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

		if(!this.inited) {
			launchGUI();
		}

		if(grid.size() == 0 || grid.get(0).size() == 0) {
			throw new IllegalArgumentException("grid width or grid height is equal to 0");
		}

		if(this.squareGridDisplayer == null || grid.size() != this.gridWidth || grid.get(0).size() != this.gridHeight) {
			setupGUI(grid.size(), grid.get(0).size());
		}

		boolean[][] booleenGrid = new boolean[grid.size()][grid.get(0).size()];
		for(int i = 0; i < grid.size(); ++i) {
			for(int j = 0; j < grid.get(i).size(); ++j) {
				booleenGrid[i][j] = grid.get(i).get(j).getValue();
			}
		}

		Platform.runLater(() -> this.squareGridDisplayer.setGrid(booleenGrid));
	}

	@Override
	public void setReadyToSetup(boolean state) {
		this.readyToSetup.set(state);
	}

	@Override
	public void setReadyToPlay(boolean state) {
		this.readyToPlay.set(state);
	}

	@Override
	public void setReadyToPause(boolean state) {
		this.readyToPause.set(state);
	}

	private int toInt(String string) {
		return string.equals("") ? 1 : Integer.parseInt(string);
	}
}
