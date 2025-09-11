/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.core;

import java.io.Serializable;

import io.sarl.lang.core.annotation.PrivateAPI;

/** A reference to a skill that could be clear dynamically and that is managing the calls to the
 * {@link Skill#install()}, {@link Skill#prepareUninstallation()} and {@link Skill#uninstall()}
 * automatically.
 *
 * <p>This class is thread-safe.
 *
 * <p>This type does not extend the {@code java.lang.reflect.Reference} because of
 * the private constructor of this later.
 *
 * <p>This type does not extend the {@code java.util.concurrent.atomic.AtomicReference} because
 * we don't want to exhibit several of its public functions.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version core 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.11
 */
public class AtomicSkillReference implements Serializable, Cloneable {

	private static final long serialVersionUID = -2985132547428365532L;

	/** Reference to the skill.
	 */
	private volatile Skill reference;

	/** Constructor.
	 *
	 * @param object the object to reference to.
	 */
	@PrivateAPI
	public AtomicSkillReference(Skill object) {
		assert object != null;
		this.reference = object;
		object.increaseReference();
	}

	@Override
	public AtomicSkillReference clone() {
		try {
			return (AtomicSkillReference) super.clone();
		} catch (CloneNotSupportedException exception) {
			throw new Error(exception);
		}
	}

	/** Returns this reference object's referent.
	 *
	 * @return the object to which this reference refers, or
	 *           {@code null} if this reference object has been cleared.
	 */
	public Skill get() {
		return this.reference;
	}

	/**
	 * Clears this reference object.
	 *
	 * @return the old reference.
	 */
	public Skill clear() {
		final var ref = this.reference;
		this.reference = null;
		if (ref != null) {
			ref.decreaseReference();
		}
		return ref;
	}

	@Override
	public String toString() {
		final var ref = this.reference;
		if (ref != null) {
			return ref.toString();
		}
		return "null"; //$NON-NLS-1$
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!obj.getClass().equals(AtomicSkillReference.class)) {
			return false;
		}
		final var ref = this.reference;
		final var aref = (AtomicSkillReference) obj;
		final var oref = aref.reference;
		if (ref == null) {
			return oref == null;
		}
		if (oref == null) {
			return false;
		}
		return ref.equals(oref);
	}

	@Override
	public int hashCode() {
		final var ref = this.reference;
		if (ref == null) {
			return 0;
		}
		return ref.hashCode();
	}

}
