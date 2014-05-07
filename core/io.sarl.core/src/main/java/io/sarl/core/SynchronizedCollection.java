/**
 * 
 */
package io.sarl.core;

import java.util.Collection;

/**
 * @author ngaud
 * @param <E> - 
 *
 */
public interface SynchronizedCollection<E> extends Collection<E> {

	
	/**
	 * Replies the mutex that is used to synchronized the access to this collection.
	 * 
	 * @return the mutex
	 */
	public Object mutex();
}
