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

package io.sarl.eclipse.notifications.sarlupdate;

import static io.sarl.eclipse.notifications.SARLCheckers.parseVersion;

import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import com.google.common.base.Strings;
import com.google.gson.Gson;
import org.arakhne.afc.vmutil.OperatingSystem;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Version;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.notifications.Notification;
import io.sarl.eclipse.notifications.NotificationDetail;
import io.sarl.eclipse.notifications.NotificationDetailMap;
import io.sarl.eclipse.notifications.Notifications;
import io.sarl.eclipse.notifications.SARLCheckers;
import io.sarl.eclipse.notifications.Sponsor;
import io.sarl.lang.core.SARLVersion;

/**
 * Check if there is a new version of SARL that is available for download. 
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 1.0
 */
public class SARLUpdateChecker {

	/** Static definition of the URL that provides a description of the
	 * available versions in JSON format.
	 */
	public static final String UPDATE_JSON_URL = "https://www.sarl.io/download/versions.json"; //$NON-NLS-1$
	//public static final String UPDATE_JSON_URL = "file:/home/sgalland/git/sarl-jekyll-website/_site/download/versions.json"; //$NON-NLS-1$

	private static final String NAME_KEY = "name"; //$NON-NLS-1$

	private static final String VERSIONS_KEY = "versions"; //$NON-NLS-1$

	private static final String VERSION_KEY = "version"; //$NON-NLS-1$

	private static final String MINIMUM_JAVA_VERSION_KEY = "min-java"; //$NON-NLS-1$

	private static final String DOWNLOAD_KEY = "download"; //$NON-NLS-1$

	private static final String DOWNLOADPAGE_KEY = "downloadPage"; //$NON-NLS-1$

	private static final String CHANGES_KEY = "changes"; //$NON-NLS-1$

	private static final String DONATE_KEY = "donate"; //$NON-NLS-1$

	private static final String URL_KEY = "url"; //$NON-NLS-1$

	private static final String LOGO_KEY = "logo"; //$NON-NLS-1$

	private static final String SPONSORPAGE_KEY = "sponsorPage"; //$NON-NLS-1$

	private static final String SPONSORS_KEY = "sponsors"; //$NON-NLS-1$

	private static final String WINDOW_DOWNLOAD_KEY = "windows"; //$NON-NLS-1$

	private static final String LINUX_DOWNLOAD_KEY = "linux"; //$NON-NLS-1$

	private static final String MAC_X86_DOWNLOAD_KEY = "mac_x86"; //$NON-NLS-1$

	private static final String MAC_ARM_DOWNLOAD_KEY = "mac_a86"; //$NON-NLS-1$

	private static final String X86_ARCH = "x86_64"; //$NON-NLS-1$

	private static final AtomicBoolean started = new AtomicBoolean();

	/** Perform asynchronous the check of the update and add a notification in {@link Notifications}.
	 */
	public static void asynchronousCheck() {
		SARLCheckers.asynchronousCheck(Messages.SARLUpdateChecker_0, started, SARLUpdateChecker::check);
	}

	/** Perform the check of the update and add a notification in {@link Notifications}.
	 *
	 * @param monitor the object for tracking the progress of the task.
	 */
	public static void check(IProgressMonitor monitor) {
		try {
			final var content = SARLCheckers.downloadString(UPDATE_JSON_URL, monitor);
			final var candidate = selectCandidate(content);
			final var notification = checkUpdate(candidate);
			if (notification != null) {
				Notifications.addNotification(notification);
			}
		} catch (Throwable exception) {
			SARLEclipsePlugin.getDefault().logWarningMessage(exception);
		}
	}
	
	private static boolean isUpgradeVersion(Version versionForLocal, Version versionForRemote) {
		return versionForLocal != null && versionForRemote != null && versionForLocal.compareTo(versionForRemote) < 0;
	}

	private static Notification checkUpdate(Map<?, ?> candidate) {
		if (candidate != null) {
			final var versionForLocal = parseVersion(SARLVersion.SARL_RELEASE_VERSION);
			final var versionForRemote = parseVersion(candidate.get(VERSION_KEY));
			if (isUpgradeVersion(versionForLocal, versionForRemote)) {

				final var details = new NotificationDetailMap();
				details.put(NotificationDetail.NAME, candidate.get(NAME_KEY));
				details.put(NotificationDetail.NEW_VERSION, versionForRemote);

				final var downloadObject = candidate.get(DOWNLOAD_KEY);
				if (downloadObject instanceof Map download) {
					final var eclipseOS = OperatingSystem.getCurrentOS();
					//
					
					switch (eclipseOS) {
					case WIN:
						details.put(NotificationDetail.DOWNLOAD, download.get(WINDOW_DOWNLOAD_KEY));
						break;
					case MACOSX:
						final var eclipseArch = Platform.getOSArch();
						if (X86_ARCH.equalsIgnoreCase(eclipseArch)) {
							details.put(NotificationDetail.DOWNLOAD, download.get(MAC_X86_DOWNLOAD_KEY));
						} else {
							details.put(NotificationDetail.DOWNLOAD, download.get(MAC_ARM_DOWNLOAD_KEY));
						}
						break;
					default:
						details.put(NotificationDetail.DOWNLOAD, download.get(LINUX_DOWNLOAD_KEY));
						break;
					}
				}

				final var downloadPageObject = candidate.get(DOWNLOADPAGE_KEY);
				if (downloadPageObject instanceof URL url) {
					details.put(NotificationDetail.DOWNLOAD_PAGE, url);
				}

				final var changesObject = candidate.get(CHANGES_KEY);
				if (changesObject instanceof URL url) {
					details.put(NotificationDetail.CHANGE_PAGE, url);
				}

				final var sponsorsObject = candidate.get(SPONSORS_KEY);
				if (sponsorsObject instanceof List list) {
					details.put(NotificationDetail.SPONSORS, list);
				}

				final var sponsorPageObject = candidate.get(SPONSORPAGE_KEY);
				if (sponsorPageObject instanceof URL url) {
					details.put(NotificationDetail.SPONSOR_PAGE, url);
				}

				final var donateObject = candidate.get(DONATE_KEY);
				if (donateObject instanceof URL url) {
					details.put(NotificationDetail.DONATE, url);
				}

				return new SARLUpdateNotification(UPDATE_JSON_URL, details);
			}
		}
		return null;
	}

	@SuppressWarnings("rawtypes")
	private static URL parseURL(Map dictionary, String key) {
		try {
			final var donateString = dictionary.get(key);
			final var uri = URI.create(donateString.toString());
			return uri.toURL();
		} catch (Exception __) {
			return null;
		}
	}
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private static Map<?, ?> selectCandidate(String content) {
		if (content != null) {
			final var gson = new Gson();
			final var dictionary = gson.fromJson(content, Map.class);
			if (dictionary == null) {
				return null;
			}

			final var downloadPage = parseURL(dictionary, DOWNLOADPAGE_KEY);
			final var changesPage = parseURL(dictionary, CHANGES_KEY);
			final var donate = parseURL(dictionary, DONATE_KEY);
			final var sponsorPage = parseURL(dictionary, SPONSORPAGE_KEY);

			final var sponsors = new ArrayList<Sponsor>();
			final var sponsorsObject = dictionary.get(SPONSORS_KEY);
			if (sponsorsObject instanceof List sponsorsList) {
				for (final var sponsorObject : sponsorsList) {
					if (sponsorObject instanceof Map sponsorDetails) {
						final var nameObject = sponsorDetails.get(NAME_KEY);
						final var name = nameObject == null ? null : Strings.emptyToNull(nameObject.toString());
						final var url = parseURL(sponsorDetails, URL_KEY);
						final var logo = parseURL(sponsorDetails, LOGO_KEY);
						if (!Strings.isNullOrEmpty(name)) {
							sponsors.add(new Sponsor(name, url, logo));
						}
					}
				}
			}

			final var versionListObject = dictionary.get(VERSIONS_KEY);
			if (versionListObject instanceof List list) {
				Map selectedCandidate = null;
				Version selectedVersion = null;
				final var minJavaVersionForLocal = parseVersion(SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT);
				for (final var candidateObject : list) {
					if (candidateObject instanceof Map candidate) {
						final var minJavaVersionForRemote = parseVersion(candidate.get(MINIMUM_JAVA_VERSION_KEY));
						if (minJavaVersionForRemote != null && minJavaVersionForLocal.compareTo(minJavaVersionForRemote) <= 0) {
							if (selectedCandidate == null || selectedVersion == null
									|| selectedVersion.compareTo(minJavaVersionForRemote) < 0) {
								selectedCandidate = candidate;
								selectedVersion = minJavaVersionForLocal;
							}
						}
					}
				}
				if (selectedCandidate != null) {
					if (downloadPage != null) {
						selectedCandidate.putIfAbsent(DOWNLOADPAGE_KEY, downloadPage);
					}
					if (changesPage != null) {
						selectedCandidate.putIfAbsent(CHANGES_KEY, changesPage);
					}
					if (donate != null) {
						selectedCandidate.putIfAbsent(DONATE_KEY, donate);
					}
					if (sponsorPage != null) {
						selectedCandidate.putIfAbsent(SPONSORPAGE_KEY, sponsorPage);
					}
					if (!sponsors.isEmpty()) {
						selectedCandidate.putIfAbsent(SPONSORS_KEY, sponsors);
					}
				}
				return selectedCandidate;
			}
		}
		return null;
	}

}
