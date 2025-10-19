/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.eclipse.notifications;

import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

/** A specific implementation of map that contains the notification details.
 * The keys are the name of the properties. The values may be of any type and
 * must be describes in the documentation of the property itself.
 *
 * <p>This map provides also function for accessing to the properties using
 * the {@link NotificationDetail predefined detail names}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 1.0
 */
public class NotificationDetailMap extends HashMap<String, Object> {

	private static final long serialVersionUID = -1312143718706545486L;

	@Override
	public Object get(Object key) {
		if (key instanceof NotificationDetail detail) {
			return super.get(detail.name());
		}
		return super.get(key);
	}

	/** Replies the value associated to the notification detail key.
	 * 
	 * @param key the detail to reply.
	 * @return the value associated to the given {@code key}, or {@code null} if none.
	 */
	public Object get(NotificationDetail key) {
		if (key == null) {
			return super.get(null);
		}
		return super.get(key.name());
	}

	/** Replies the list of sponsors.
	 *
	 * @return the list of sponsors.
	 */
	@SuppressWarnings("unchecked")
	public List<Sponsor> getSponsors() {
		final var sponsors = super.get(NotificationDetail.SPONSORS.name());
		if (sponsors instanceof List) {
			return (List<Sponsor>) sponsors;
		}
		return null;
	}

	/** Replies the URL to the Internet page dedicated to the sponsors.
	 *
	 * @return the URL to the sponsor page.
	 */
	public URL getSponsorPage() {
		final var sponsor = super.get(NotificationDetail.SPONSOR_PAGE.name());
		if (sponsor instanceof URL url) {
			return url;
		}
		return null;
	}

	/** Replies the donate URL.
	 *
	 * @return the URL to the donate page.
	 */
	public URL getDonate() {
		final var donate = super.get(NotificationDetail.DONATE.name());
		if (donate instanceof URL url) {
			return url;
		}
		return null;
	}

	@Override
	public boolean containsKey(Object key) {
		if (key instanceof NotificationDetail detail) {
			return super.containsKey(detail.name());
		}
		return super.containsKey(key);
	}

	/** Put the notification property in the map.
	 *
	 * @param key the name of the notification detail.
	 * @param value the value
	 * @return the value that was present in the map before changing it.
	 */
	public Object put(NotificationDetail key, Object value) {
		return super.put(key.name(), value);
	}

	@Override
	public Object getOrDefault(Object key, Object defaultValue) {
		if (key instanceof NotificationDetail detail) {
			return super.getOrDefault(detail.name(), defaultValue);
		}
		return super.getOrDefault(key, defaultValue);
	}

	/** Put the notification property in the map only if it was not yet already present in this map.
	 *
	 * @param key the name of the notification detail.
	 * @param value the value
	 * @return the value that was present in the map before changing it.
	 */
	public Object putIfAbsent(NotificationDetail key, Object value) {
		return super.putIfAbsent(key.name(), value);
	}

	/** Replies the notification property if it is in the map, or compute
	 * its value using {@code mappingFunction}, save it in the map and
	 * return it.
	 *
	 * @param key the name of the notification detail.
	 * @param mappingFunction the function that must be used for computing the value. The
	 *      argument of this function is the {@code key}.
	 * @return the value that is in the map.
	 */
	public Object computeIfAbsent(NotificationDetail key, Function<? super String, ? extends Object> mappingFunction) {
		return super.computeIfAbsent(key.name(), mappingFunction);
	}

	/** If the property is inside the map, compute a new value with {@code remappingFunction}, save it 
	 * in the map and return it. Otherwise, return {@code null}.
	 *
	 * @param key the name of the notification detail.
	 * @param remappingFunction the function that must be used for recomputing the value. The
	 *      first argument of this function is the {@code key}. The second argument of this
	 *      function is the value previously stored in the map. If this function returns {@code null}
	 *      the property is removed from the map.
	 * @return the value that is in the map.
	 */
	public Object computeIfPresent(NotificationDetail key,
			BiFunction<? super String, ? super Object, ? extends Object> remappingFunction) {
		return super.computeIfPresent(key.name(), remappingFunction);
	}

	/** Attempts to compute a mapping for the specified key and its current mapped value
	 * (or {@code null} if there is no current mapping).
	 *
	 * <p><pre><code>
	 * map.compute(key, (k, v) -> (v == null) ? msg : v.concat(msg))
	 * </code></pre>
	 *
	 * <p>If the remapping function returns null, the mapping is removed (or
	 * remains absent if initially absent). If the remapping function itself
	 * throws an (unchecked) exception, the exception is rethrown, and the current
	 * mapping is left unchanged.
	 *
	 * <p>The remapping function should not modify this map during computation.
	 *
	 * @param key the name of the notification detail.
	 * @param remappingFunction the function that must be used for recomputing the value. The
	 *      first argument of this function is the {@code key}. The second argument of this
	 *      function is the value previously stored in the map. If this function returns {@code null}
	 *      the property is removed from the map.
	 * @return the value that is in the map.
	 */
	public Object compute(NotificationDetail key, BiFunction<? super String, ? super Object, ? extends Object> remappingFunction) {
		return super.compute(key.name(), remappingFunction);
	}

}
