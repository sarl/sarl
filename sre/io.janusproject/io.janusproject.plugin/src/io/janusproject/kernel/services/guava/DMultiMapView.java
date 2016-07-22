/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.janusproject.kernel.services.guava;

import com.google.common.collect.Multimap;
import io.janusproject.util.AbstractDMultiMapView;

/**
 * A view on a Guava multimap that provides the API for the DMultiMap.
 *
 * @param <K> - type of the keys.
 * @param <V> - type of the values.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class DMultiMapView<K, V> extends AbstractDMultiMapView<K, V> {

	private static final long serialVersionUID = -6970650402150118406L;

	private final Multimap<K, V> map;

	/**
	 * @param name - the name of the multimap.
	 * @param map - the multimap.
	 */
	public DMultiMapView(String name, Multimap<K, V> map) {
		super(name);
		assert (map != null);
		this.map = map;
	}

	@Override
	public Multimap<K, V> getDelegatedObject() {
		return this.map;
	}

}
