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

/** The id of a notification detail that is supported by the standard SARL notification system.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 1.0
 */
public enum NotificationDetail {

	/** Name of the component that has changed.
	 */
	NAME,

	/** Number of the new version that is available.
	 */
	NEW_VERSION,

	/** URL to the download system directly.
	 */
	DOWNLOAD,

	/** URL to the Internet page for downloading.
	 */
	DOWNLOAD_PAGE,

	/** URL to the Internet page that describes the changes in the new version.
	 */
	CHANGE_PAGE,

	/** Link to the donation system.
	 */
	DONATE,

	/** Link to the Internet page that is dedicated to the sponsors.
	 */
	SPONSOR_PAGE,

	/** List of sponsors.
	 */
	SPONSORS;

}
