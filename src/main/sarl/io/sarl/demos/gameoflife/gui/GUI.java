package io.sarl.demos.gameoflife.gui;

import io.sarl.demos.gameoflife.environment.agent.EnvironmentListener;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;
import org.eclipse.xtext.xbase.lib.Pair;

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
	private final Label widthLabel = new Label("Width:");
	private final RestrictedNumberTextField widthTextField = new RestrictedNumberTextField(5, 1, 100);
	private final HBox widthHBox = new HBox(this.widthLabel, this.widthTextField);
	private final Label heightLabel = new Label("Height:");
	private final RestrictedNumberTextField heightTextField = new RestrictedNumberTextField(5, 1, 100);
	private final HBox heightHBox = new HBox(this.heightLabel, this.heightTextField);
	private final Button setupButton = new Button("Setup");
	private final Label periodLabel = new Label("Period:");
	private final RestrictedNumberTextField periodTextField = new RestrictedNumberTextField(5, 1, 1000);
	private final Label periodUnitytLabel = new Label("ms");
	private final HBox periodHBox = new HBox(this.periodLabel, this.periodTextField, this.periodUnitytLabel);
	private final Button playButton = new Button("Play");
	private final Button pauseButton = new Button("Pause");
	private final Button exitButton = new Button("Exit");
	private final HBox buttonsHbox = new HBox(this.widthHBox, this.heightHBox, this.setupButton, this.periodHBox, this.playButton, this.pauseButton, this.exitButton);
	private SquareGridDisplayer squareGridDisplayer;
	private final Pane squareGridDisplayerPane = new Pane();
	private final VBox vBox = new VBox(this.buttonsHbox, this.squareGridDisplayerPane);
	private boolean inited = false;
	private int gridWidth;
	private int gridHeight;
	private SimpleBooleanProperty readyToPlay = new SimpleBooleanProperty(true);
	private SimpleBooleanProperty readyToPause = new SimpleBooleanProperty(true);
	private SimpleBooleanProperty readyToSetup = new SimpleBooleanProperty(true);

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
		if(!this.inited) {

			this.setupButton.disableProperty().bind(Bindings.not(this.readyToSetup));
			this.setupButton.setOnAction(new EventHandler<ActionEvent>() {
				@Override
				public void handle(ActionEvent event) {
					GUI.this.gridWidth = Integer.parseInt(GUI.this.widthLabel.getText());
					GUI.this.gridHeight = Integer.parseInt(GUI.this.heightLabel.getText());
					for(GUIListener listener : GUI.this.listeners) {
						listener.setup(GUI.this.gridWidth, GUI.this.gridHeight);
					}
				}
			});

			//TODO: check if textfield update are cheched with onAction
			this.periodTextField.setOnAction(new EventHandler<ActionEvent>() {
				@Override
				public void handle(ActionEvent event) {
					for(GUIListener listener : GUI.this.listeners) {
						listener.periodUpdated(Integer.parseInt(GUI.this.periodTextField.getText()));
					}
				}
			});

			this.playButton.disableProperty().bind(Bindings.not(this.readyToPlay));
			this.playButton.setOnAction(new EventHandler<ActionEvent>() {
				@Override
				public void handle(ActionEvent event) {
					GUI.this.listeners.forEach(GUIListener::play);
				}
			});

			this.pauseButton.disableProperty().bind(Bindings.not(this.readyToPause));
			this.pauseButton.setOnAction(new EventHandler<ActionEvent>() {
				@Override
				public void handle(ActionEvent event) {
					GUI.this.listeners.forEach(GUIListener::pause);
				}
			});

			this.exitButton.setOnAction(new EventHandler<ActionEvent>() {
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
			this.primaryStage.setTitle("Sarl game of life demo");

			Scene scene = new Scene(this.vBox);

			this.primaryStage.setScene(scene);
			this.primaryStage.show();
			this.inited = true;
		}
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
				this.squareGridDisplayerPane.getChildren().removeAll();
				this.squareGridDisplayerPane.getChildren().add(this.squareGridDisplayer);
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

		if(squareGridDisplayer == null || grid.size() != this.gridWidth || grid.get(0).size() != this.gridHeight) {
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
}
