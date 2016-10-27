package io.sarl.demos.gameoflife.gui;

import io.sarl.demos.gameoflife.environment.agent.EnvironmentListener;
import javafx.application.Application;
import javafx.application.Platform;
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
public class GUI extends Application implements EnvironmentListener {

	private static GUI gui;
	private final List<GUIListener> listeners = new ArrayList<>();
	private Stage primaryStage;
	private final Label widthLabel = new Label("Width:");
	private final RestrictedNumberTextField widthTextField = new RestrictedNumberTextField(5, 1, 100);
	private final Label heightLabel = new Label("Height:");
	private final RestrictedNumberTextField heightTextField = new RestrictedNumberTextField(5, 1, 100);
	private final Button setupButton = new Button("Setup");
	private final Button playButton = new Button("Play");
	private final Button pauseButton = new Button("Pause");
	private final Button exitButton = new Button("Exit");
	private final HBox buttonsHbox = new HBox(this.widthLabel, this.widthTextField, this.heightLabel, this.heightTextField, this.setupButton, this.playButton, this.pauseButton, this.exitButton);
	private SquareGridDisplayer squareGridDisplayer;
	private final Pane squareGridDisplayerPane = new Pane();
	private final VBox vBox = new VBox(this.buttonsHbox, this.squareGridDisplayerPane);
	private boolean inited = false;
	private int gridWidth;
	private int gridHeight;

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

		if(grid.size() != this.gridWidth || grid.get(0).size() != this.gridHeight) {
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
}
