/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

package io.sarl.eclipse.launching.dialog;

/**
 * Interace for representing any tab for the SARL run-time environment configuration.
 *
 * @author <a href="http://www.ciad-lab.fr/author-10836/">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.12.0 20210527-171007
 * @mavengroupid io.sarl
 * @mavenartifactid io.sarl.eclipse
 * @since 0.12
 */
public interface ISarlRuntimeEnvironmentTab {

	/** Add the listener on the SRE changes.
	 *
	 * @param listener the listener.
	 */
	void addSreChangeListener(ISreChangeListener listener);

	/** Remove the listener on the SRE changes.
	 *
	 * @param listener the listener.
	 */
	void removeSreChangeListener(ISreChangeListener listener);

}
