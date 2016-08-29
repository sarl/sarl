/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.janusproject.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.UUID;

import io.janusproject.services.distributeddata.DMapListener;
import io.janusproject.testutils.AbstractJanusTest;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class AbstractMapViewTest extends AbstractJanusTest {

	@Nullable
	private ViewMock view;

	@Before
	public void setUp() {
		this.view = new ViewMock<String, String>();
	}

	@Test
	public void addDMapListener_0() {
		DMapListener<String, String> listener = mock(DMapListener.class);
		this.view.addDMapListener(listener);
		ListenerCollection<DMapListener<? super String, ? super String>> listeners = this.view.testGetListeners();
		assertEquals(1, listeners.getListenerCount(DMapListener.class));
		DMapListener<? super String, ? super String>[] array = listeners.getListeners(DMapListener.class);
		assertNotNull(array);
		assertEquals(1, array.length);
		assertSame(listener, array[0]);
	}

	@Test
	public void addDMapListener_1() {
		DMapListener<String, String> listener1 = mock(DMapListener.class);
		DMapListener<String, String> listener2 = mock(DMapListener.class);
		this.view.addDMapListener(listener1);
		this.view.addDMapListener(listener2);
		ListenerCollection<DMapListener<? super String, ? super String>> listeners = this.view.testGetListeners();
		assertEquals(2, listeners.getListenerCount(DMapListener.class));
		DMapListener<? super String, ? super String>[] array = listeners.getListeners(DMapListener.class);
		assertNotNull(array);
		assertEquals(2, array.length);
		if (array[0] == listener1) {
			assertSame(listener2, array[1]);
		} else {
			assertSame(listener2, array[0]);
			assertSame(listener1, array[1]);
		}
	}

	@Test
	public void removeDMapListener() {
		DMapListener<String, String> listener1 = mock(DMapListener.class);
		DMapListener<String, String> listener2 = mock(DMapListener.class);
		this.view.addDMapListener(listener1);
		this.view.addDMapListener(listener2);
		this.view.removeDMapListener(listener1);
		ListenerCollection<DMapListener<? super String, ? super String>> listeners = this.view.testGetListeners();
		assertEquals(1, listeners.getListenerCount(DMapListener.class));
		DMapListener<? super String, ? super String>[] array = listeners.getListeners(DMapListener.class);
		assertNotNull(array);
		assertEquals(1, array.length);
		assertSame(listener2, array[0]);
	}

	@Test
	public void fireEntryAdded() {
		String key = UUID.randomUUID().toString();
		String value = UUID.randomUUID().toString();
		DMapListener<String, String> listener = mock(DMapListener.class);
		this.view.addDMapListener(listener);
		this.view.fireEntryAdded(key, value);
		ArgumentCaptor<String> arg0 = ArgumentCaptor.forClass(String.class);
		ArgumentCaptor<String> arg1 = ArgumentCaptor.forClass(String.class);
		verify(listener, times(1)).entryAdded(arg0.capture(), arg1.capture());
		assertSame(key, arg0.getValue());
		assertSame(value, arg1.getValue());
	}

	@Test
	public void fireEntryRemoved() {
		String key = UUID.randomUUID().toString();
		String value = UUID.randomUUID().toString();
		DMapListener<String, String> listener = mock(DMapListener.class);
		this.view.addDMapListener(listener);
		this.view.fireEntryRemoved(key, value);
		ArgumentCaptor<String> arg0 = ArgumentCaptor.forClass(String.class);
		ArgumentCaptor<String> arg1 = ArgumentCaptor.forClass(String.class);
		verify(listener, times(1)).entryRemoved(arg0.capture(), arg1.capture());
		assertSame(key, arg0.getValue());
		assertSame(value, arg1.getValue());
	}

	@Test
	public void fireEntryUpdated() {
		String key = UUID.randomUUID().toString();
		String value = UUID.randomUUID().toString();
		DMapListener<String, String> listener = mock(DMapListener.class);
		this.view.addDMapListener(listener);
		this.view.fireEntryUpdated(key, value);
		ArgumentCaptor<String> arg0 = ArgumentCaptor.forClass(String.class);
		ArgumentCaptor<String> arg1 = ArgumentCaptor.forClass(String.class);
		verify(listener, times(1)).entryUpdated(arg0.capture(), arg1.capture());
		assertSame(key, arg0.getValue());
		assertSame(value, arg1.getValue());
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class ViewMock<K, V> extends AbstractMapView<K, V> {

		/**
		 */
		public ViewMock() {
			//
		}

		public ListenerCollection<DMapListener<? super K, ? super V>> testGetListeners() {
			return this.listeners;
		}

		@Override
		public void fireEntryAdded(K key, V value) {
			super.fireEntryAdded(key, value);
		}

		@Override
		public void fireEntryRemoved(K key, V value) {
			super.fireEntryRemoved(key, value);
		}

		@Override
		public void fireEntryUpdated(K key, V value) {
			super.fireEntryUpdated(key, value);
		}

	}

}
