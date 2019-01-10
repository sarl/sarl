/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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
package io.sarl.maven.sre;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.verify;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.junit.ComparisonFailure;
import org.junit.Test;
import org.mockito.ArgumentCaptor;

import io.sarl.eclipse.runtime.SREConstants;
import io.sarl.maven.sre.AbstractSREMojo.ManifestUpdater;

@SuppressWarnings("all")
public class AbstractSREMojoSTest {

	private static final String GENERAL_PREFIX = "MANIFEST_"; //$NON-NLS-1$

	/** Test if the abstract mojo is creating a manifest attribute for each attribute
	 * in {@link SREConstants}
	 */
	@Test
	public void constantCoverageForManifestCreation() throws Exception {
		final ManifestUpdater update = mock(ManifestUpdater.class);
		final AbstractSREMojo moj = spy(new AbstractSREMojo() {
			{
				setMainClass("io.sarl.maven.sre.tests.Fake");
				setManifestUpdater(update);
				setLog(mock(Log.class));
			}
			@Override
			protected void executeMojo() throws MojoExecutionException, MojoFailureException {
				//
			}
		});

		final Set<String> sectionAttributes = new HashSet<>();
		final Set<String> mainAttributes = new HashSet<>();
		for (final Field field : SREConstants.class.getDeclaredFields()) {
			if (field.getName().startsWith(GENERAL_PREFIX)
					&& !field.getName().equals("MANIFEST_SECTION_SRE")) {
				if (field.getName().equals("MANIFEST_MAIN_CLASS")
						|| field.getName().equals("MANIFEST_CLASS_PATH")) {
					mainAttributes.add(field.get(null).toString());
				} else {
					sectionAttributes.add(field.get(null).toString());
				}
			}
		}

		moj.createSREManifest();
				
		ArgumentCaptor<String> nameCaptor1 = ArgumentCaptor.forClass(String.class);
		ArgumentCaptor<String> valueCaptor1 = ArgumentCaptor.forClass(String.class);
		verify(update, times(mainAttributes.size())).addMainAttribute(nameCaptor1.capture(), valueCaptor1.capture());

		ArgumentCaptor<String> nameCaptor2 = ArgumentCaptor.forClass(String.class);
		ArgumentCaptor<String> valueCaptor2 = ArgumentCaptor.forClass(String.class);
		verify(update, times(sectionAttributes.size())).addSREAttribute(nameCaptor2.capture(), valueCaptor2.capture());

		assertCollectionEquals(mainAttributes, nameCaptor1.getAllValues());
		assertCollectionEquals(sectionAttributes, nameCaptor2.getAllValues());
	}

	private static <T> void assertCollectionEquals(Collection<T> expected, Iterable<T> actual) {
		if (actual == null) {
			throw new ComparisonFailure("not same collections", Objects.toString(expected), Objects.toString(actual));
		}
		final List<T> list = new ArrayList<>(expected);
		final Iterator<T> iterator = actual.iterator();
		while (iterator.hasNext()) {
			final T current = iterator.next();
			if (!list.remove(current)) {
				throw new ComparisonFailure("unexpected element: " + current, Objects.toString(expected), Objects.toString(actual));
			}
		}
		if (!list.isEmpty()) {
			throw new ComparisonFailure("element not found", Objects.toString(expected), Objects.toString(actual));
		}
	}
}
