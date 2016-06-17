/**
 * 
 */
package io.sarl.lang.core;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Wraps an event that was posted, but which had no subscribers and thus could not be delivered.
 *
 * <p>
 * Registering a DeadEvent {@code BehaviorGuardEvaluator} is useful for debugging or logging, as it can detect misconfigurations in a system's event distribution.
 *
 * Directly copy from com.google.common.eventbus.DeadEvent
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */

public final class DeadEvent extends Event {

	private static final long serialVersionUID = 1117818766135181831L;
	
	private final Object event;

	/**
	 * Creates a new DeadEvent.
	 *
	 * @param event the event that could not be delivered.
	 */
	public DeadEvent(Event event) {
		super(event.getSource());
		this.event = checkNotNull(event);
	}


	/**
	 * Returns the wrapped, 'dead' event, which the system was unable to deliver to any registered {@code BehaviorGuardEvaluator}.
	 *
	 * @return the 'dead' event that could not be delivered.
	 */
	public Object getEvent() {
		return this.event;
	}


}
