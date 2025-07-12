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

package io.sarl.bspl.lang.bspl.impl;

/**
 * <!-- begin-user-doc -->
 * Custom implementation of the BSPL role declaration.
 * <!-- end-user-doc -->
 *
 * <p>The following features are implemented:
 * <ul>
 *   <li>{@link #setMinMax(Integer, Integer)} <em>Set min and max cardinalities and ensures a correct order of min and max cardinalities</em></li>
 *   <li>{@link #setMin(Integer)} <em>Change the min cardinality and ensures a correct order of min and max cardinalities</em></li>
 *   <li>{@link #setMax(Integer)} <em>Change the max cardinality and ensures a correct order of min and max cardinalities</em></li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class BsplProtocolRoleImplCustom extends BsplProtocolRoleImpl {

	protected boolean fixedMinMax;
	
	/**
	 * <!-- begin-user-doc -->
	 * Constructor.
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected BsplProtocolRoleImplCustom() {
		super();
	}

	private static Integer ensurePositiveOrZero(Integer value) {
		if (value != null && value.intValue() < 0) {
			return Integer.valueOf(0);
		}
		return value;
	}

	@Override
	public void setMinMax(Integer newMin, Integer newMax) {
		final var nmin = ensurePositiveOrZero(newMin);
		final var nmax = ensurePositiveOrZero(newMax);
		this.fixedMinMax = nmin != null && nmax != null && nmin.intValue() > nmax.intValue();
		if (this.fixedMinMax) {
			super.setMin(nmax);
			super.setMax(nmin);
		} else {
			super.setMin(nmin);
			super.setMax(nmax);
		}
	}
	
	@Override
	public void setMin(Integer newMin) {
		if (newMin != null) {
			setMinMax(newMin, getMax());
		} else {
			this.fixedMinMax = false;
			super.setMin(newMin);
		}
	}

	@Override
	public void setMax(Integer newMax) {
		if (newMax != null) {
			setMinMax(getMin(), newMax);
		} else {
			this.fixedMinMax = false;
			super.setMax(newMax);
		}
	}

	@Override
	public boolean isFixedMinMax() {
		return this.fixedMinMax;
	}
	
}
