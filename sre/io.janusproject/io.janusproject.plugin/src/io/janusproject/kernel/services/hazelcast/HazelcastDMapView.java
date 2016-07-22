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

package io.janusproject.kernel.services.hazelcast;

import com.hazelcast.core.IMap;
import io.janusproject.util.AbstractDMapView;

/**
 * View from a Hazelcast map to DMap.
 *
 * @param <K> - type of the keys.
 * @param <V> - type of the values.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class HazelcastDMapView<K, V> extends AbstractDMapView<K, V> {

	private static final long serialVersionUID = 6183890399347925540L;

	private final IMap<K, V> map;

	/**
	 * @param map - the underground map to use.
	 */
	public HazelcastDMapView(IMap<K, V> map) {
		super(map.getName());
		this.map = map;
	}

	@Override
	public boolean isBackedCollection() {
		return false;
	}

	@Override
	public IMap<K, V> getDelegatedObject() {
		return this.map;
	}

}
