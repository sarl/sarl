/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
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

package io.sarl.api.core.tests.spaces;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.UUID;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.sarl.api.core.spaces.AbstractSpace;
import io.sarl.api.core.spaces.OpenEventSpaceSpecification;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.tests.api.extensions.JavaVersionCheckExtension;
import io.sarl.tests.api.extensions.ContextInitExtension;
import io.sarl.tests.api.extensions.PropertyRestoreExtension;

/** 
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@ExtendWith({
	ContextInitExtension.class,
	JavaVersionCheckExtension.class,
	PropertyRestoreExtension.class
})
@DisplayName("AbstractSpace")
@Tag("unit")
@Tag("api")
@SuppressWarnings("all")
public class XAbstractSpaceTest {

	@Test
	@DisplayName("getSpaceID")
	public void getSpaceID() {
		var spaceId = new SpaceID(UUID.randomUUID(), UUID.randomUUID(), OpenEventSpaceSpecification.class);
		var base = new AbstractSpace(spaceId) {

			@Override
			public void forEachStrongParticipant(org.eclipse.xtext.xbase.lib.Procedures.Procedure1<? super UUID> callback) {
				throw new UnsupportedOperationException();
			}

			@Override
			public void forEachWeakParticipant(org.eclipse.xtext.xbase.lib.Procedures.Procedure1<? super UUID> callback) {
				throw new UnsupportedOperationException();
			}

			@Override
			public int getNumberOfStrongParticipants() {
				throw new UnsupportedOperationException();
			}
			
			@Override
			public int getNumberOfWeakParticipants() {
				throw new UnsupportedOperationException();
			}

			@Override
			public boolean isPseudoEmpty(UUID id) {
				throw new UnsupportedOperationException();
			}
			
		};
		assertEquals(spaceId, base.getSpaceID());
	}

}
