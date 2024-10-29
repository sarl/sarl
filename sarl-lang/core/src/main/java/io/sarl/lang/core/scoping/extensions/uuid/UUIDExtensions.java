/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.core.scoping.extensions.uuid;

import java.util.UUID;

import com.google.common.annotations.GwtCompatible;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.core.Address;

/**
 * Static methods to add to the scope of {@code UUID}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.14
 */
@GwtCompatible
public final class UUIDExtensions {

	private UUIDExtensions() {
		//
	}

	/** Replies if the given identifier is equal to the identifier in the given identifier.
	 * It is equivalent to {@code id.equals(address.getID())}.
	 *
	 * @param id the identifier to test.
	 * @param address the address to test.
	 * @return {@code true} if the identifier is equal to the address' identifier.
	 */
	@Pure
	@Inline(value = "((($1) == null && ($2) == null) || (($1) != null && ($1).equals(($2).getID())))", statementExpression = false)
	public static boolean operator_equals(UUID id, Address address) {
		return (id == null && address == null) || (id != null && id.equals(address.getID()));
	}

	/** Replies if the given identifier is not equal to the identifier in the given identifier.
	 * It is equivalent to {@code !id.equals(address.getID())}.
	 *
	 * @param id the identifier to test.
	 * @param address the address to test.
	 * @return {@code false} if the identifier is equal to the address' identifier.
	 */
	@Pure
	@Inline(value = "((($1) != null || ($2) != null) && (($1) == null || !($1).equals(($2).getID())))", statementExpression = false)
	public static boolean operator_notEquals(UUID id, Address address) {
		return (id != null || address != null) && (id == null || !id.equals(address.getID()));
	}

}
