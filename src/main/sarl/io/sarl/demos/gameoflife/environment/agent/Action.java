package io.sarl.demos.gameoflife.environment.agent;

import java.util.UUID;

import org.arakhne.afc.math.geometry.d2.i.Vector2i;

public class Action {
	public enum Type {
		CREATE(true),
		DESTROY(false);
		
		public final boolean value;
		Type(boolean val) {
			this.value = val;
		}
	}
	
	private final UUID emitter;
	private final Vector2i position;
	private final Type type;
	
	Action(UUID emitter, Vector2i position, Type type) {
		this.emitter = emitter;
		this.position = position;
		this.type = type;
	}
	
	/**
	 * Replies the emitter.
	 * @return
	 */
	public UUID getEmitter() {
		return this.emitter;
	}
	
	/**
	 * Replies the position of the emitter.
	 * @return
	 */
	public Vector2i getPosition() {
		return this.position;
	}

	/**
	 * Replies the type of the event.
	 * @return
	 */
	public Type getType() {
		return type;
	}
}
