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

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.text.MessageFormat;
import java.util.List;
import java.util.regex.Pattern;

import com.google.common.base.Strings;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.browser.LocationListener;
import org.eclipse.swt.browser.ProgressEvent;
import org.eclipse.swt.browser.ProgressListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.part.ViewPart;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.util.UIUtils;

/**
 * A view that inform the user for news about the SARL environment. The content of the browser depends on the detected news.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 1.0
 */
public class SARLNotificationViewPart extends ViewPart {

	/** Identifier of the view.
	 */
	public static final String VIEW_ID = "io.sarl.eclipse.notifications.NotificationView"; //$NON-NLS-1$

	private static final String HTML_TEMPLATE = "html/Notifications.html"; //$NON-NLS-1$
	
	private static final String CSS_TEMPLATE = "html/Notifications.css"; //$NON-NLS-1$

	private static final String TITLE_KEY = "@@@TITLE@@@"; //$NON-NLS-1$

	private static final String PRODUCTNAME_KEY = "@@@PRODUCTNAME@@@"; //$NON-NLS-1$

	private static final String CSSCONTENT_KEY = "@@@CSSCONTENT@@@"; //$NON-NLS-1$

	private static final String VERSION_KEY = "@@@VERSION@@@"; //$NON-NLS-1$

	private static final String DOWNLOAD_KEY = "@@@DOWNLOADLINK@@@"; //$NON-NLS-1$

	private static final String SPONSORPAGE_KEY = "@@@SPONSORLINK@@@"; //$NON-NLS-1$

	private static final String SPONSORS_KEY = "@@@SPONSORS@@@"; //$NON-NLS-1$
	
	private static final String HIDESPONSORS_KEY = "@@@HIDESPONSORS@@@"; //$NON-NLS-1$

	private Browser browser;

	private Notification notification;

	/** Hide this view.
	 */
	public void hide() {
		getSite().getPage().hideView(this);
	}

	/** Replies the browser that is inside this view.
	 *
	 * @return the browser. It may be {@code null} if the browser was not already created.
	 */
	protected Browser getBrowser() {
		return this.browser;
	}

	/** Replies the notification associated to this view.
	 *
	 * @return the notification; or {@code null} if there is not notification associated.
	 */
	public Notification getNotification() {
		return this.notification;
	}

	/** Change the notification associated to this view.
	 *
	 * @param notification the notification.
	 */
	public void setNotification(Notification notification) {
		this.notification = notification;
		updateContent();
	}

	private static String readCss() {
		try {
		    final var fileURL = FileLocator.find(SARLEclipsePlugin.getDefault().getBundle(), new Path(CSS_TEMPLATE), null);
		    final var buffer = new StringBuilder();
		    if (fileURL != null) {
			    try (final var is = new BufferedReader(new InputStreamReader(fileURL.openStream()))) {
			    	var line = is.readLine();
			    	while (line != null) {
			    		buffer.append(line).append("\n"); //$NON-NLS-1$
			    		line = is.readLine();
			    	}
			    }
		    }
		    return buffer.toString();
	    } catch (Throwable ex) {
	    	SARLEclipsePlugin.getDefault().logWarningMessage(ex);
	    	return ""; //$NON-NLS-1$
	    }
	}
	
	private String readHtmlTemplate(Notification notification) throws IOException {
	    final var fileURL = FileLocator.find(SARLEclipsePlugin.getDefault().getBundle(), new Path(HTML_TEMPLATE), null);
	    if (fileURL == null) {
	    	throw new FileNotFoundException(HTML_TEMPLATE);
	    }
	    final var cssContent = readCss();
	    final var buffer = new StringBuilder();
	    try (final var is = new BufferedReader(new InputStreamReader(fileURL.openStream()))) {
	    	var line = is.readLine();
	    	while (line != null) {
	    		buffer.append(line).append("\n"); //$NON-NLS-1$
	    		line = is.readLine();
	    	}
	    }
		return replaceMetaValues(notification, cssContent, buffer.toString());
	}

	private String replaceMetaValues(Notification notification, String cssContent, String input) {
		final var name = notification.getDetails().get(NotificationDetail.NAME);
		final var nameStr = name == null ? "" : name.toString(); //$NON-NLS-1$
		var output = input.replaceAll(Pattern.quote(PRODUCTNAME_KEY), nameStr);

		final var version = notification.getDetails().get(NotificationDetail.NEW_VERSION);
		final var versionStr = version == null ? "" : version.toString(); //$NON-NLS-1$
		output = output.replaceAll(Pattern.quote(VERSION_KEY), versionStr);

		final var download = notification.getDetails().get(NotificationDetail.DOWNLOAD);
		output = output.replaceAll(Pattern.quote(DOWNLOAD_KEY), download == null ? "" : download.toString()); //$NON-NLS-1$

		final var sponsorPage = notification.getDetails().getSponsorPage();
		output = output.replaceAll(Pattern.quote(SPONSORPAGE_KEY), sponsorPage == null ? "" : sponsorPage.toString()); //$NON-NLS-1$

		final var title = MessageFormat.format(Messages.SARLNotificationViewPart_0, nameStr, versionStr);
		output = output.replaceAll(Pattern.quote(TITLE_KEY), title);

		output = output.replaceAll(Pattern.quote(CSSCONTENT_KEY), cssContent);

		final var sponsors = notification.getDetails().getSponsors();
		output = output.replaceAll(Pattern.quote(SPONSORS_KEY), getHtmlCodeForSponsorList(sponsors));

		if (sponsorPage == null && sponsors.isEmpty()) {
			output = output.replaceAll(Pattern.quote(HIDESPONSORS_KEY), getHtmlCodeForHidingSponsorSection());
		} else {
			output = output.replaceAll(Pattern.quote(HIDESPONSORS_KEY), ""); //$NON-NLS-1$
		}

		return output;

	}

	/** Replies the HTML code that enables to hide the sponsor section.
	 *
	 * @return HTML code
	 */
	@SuppressWarnings("static-method")
	protected String getHtmlCodeForHidingSponsorSection() {
		return "style=\"display: none\""; //$NON-NLS-1$
	}

	/** Replies the HTML code that shows the list of sponsors.
	 *
	 * @param sponsors the list of sponsors.
	 * @return HTML code
	 */
	@SuppressWarnings("static-method")
	protected String getHtmlCodeForSponsorList(List<Sponsor> sponsors) {
		final var code = new StringBuilder();
		for (final var sponsor : sponsors) {
			final var logo = sponsor.logo();
			if (logo != null) {
				final var url = sponsor.url();
				code.append("<span class=\"ide-wg-member-logo-container\">"); //$NON-NLS-1$
				if (url != null) {
					code.append("<a href=\"eclipse+external:"); //$NON-NLS-1$
					code.append(url.toString());
					code.append("\">"); //$NON-NLS-1$
				}
				code.append("<img class=\"ide-wg-member-logo\" src=\""); //$NON-NLS-1$
				code.append(logo.toString());
				code.append("\">"); //$NON-NLS-1$
				if (url != null) {
					code.append("</a>"); //$NON-NLS-1$
				}
				code.append("</span>"); //$NON-NLS-1$
			}
		}
		return code.toString();
	}

	private void updateContent() {
		if (this.browser != null && this.notification != null) {
			try {
				this.browser.setText(readHtmlTemplate(this.notification), true);
			} catch (IOException ex) {
				SARLEclipsePlugin.getDefault().logWarningMessage(ex);
			}
		}
	}

	@Override
	public void createPartControl(Composite parent) {
		this.browser = new Browser(parent, SWT.NONE);
		this.browser.setJavascriptEnabled(true);

		this.browser.addTitleListener(event -> {
			if (!event.title.isBlank()) {
				setPartName(event.title);
			}
		});

		this.browser.addProgressListener(new ProgressListener() {
			@Override
			public void completed(ProgressEvent event) {
				final var brw = getBrowser();
				brw.removeProgressListener(this);
				brw.setVisible(true);
				UIUtils.asyncExec(parent, () -> {
					brw.addLocationListener(new LocationListener() {
						@Override
						public void changing(LocationEvent event) {
							handleChanging(event);
						}

						@Override
						public void changed(LocationEvent event) {
							//
						}
					});
				});
			}

			@Override
			public void changed(ProgressEvent event) {
				//
			}
		});

		this.browser.setVisible(false);
	}

	@Override
	public void setFocus() {
		if (this.browser != null) {
			this.browser.setFocus();
		}
	}

	/** Resolve the given Eclipse URI and replies the target URI.
	 *
	 * @param location the Eclipse URI to be resolved.
	 * @return the target URI.
	 */
	@SuppressWarnings("static-method")
	protected URI resolveEclipseURI(URI location) {
		var path = location.getPath();
		if (!Strings.isNullOrEmpty(path)) {
			return URI.create(path);
		}
		path = location.getRawPath();
		if (!Strings.isNullOrEmpty(path)) {
			return URI.create(path);
		}
		path = location.getSchemeSpecificPart();
		if (!Strings.isNullOrEmpty(path)) {
			return URI.create(path);
		}
		path = location.getRawSchemeSpecificPart();
		if (!Strings.isNullOrEmpty(path)) {
			return URI.create(path);
		}
		return location;
	}

	/** Invoked when a hyperef link is clicked in the browser.
	 *
	 * @param event the description of the click event.
	 */
	protected void handleChanging(LocationEvent event) {
		final URI location;
		try {
			location = URI.create(event.location);
		} catch (Throwable ex) {
			SARLEclipsePlugin.getDefault().logWarningMessage("Invalid URI format: " + event.location, ex); //$NON-NLS-1$
			event.doit = false;
			return;
		}
		final var scheme = location.getScheme();
		if (scheme != null && scheme.startsWith("eclipse+")) { //$NON-NLS-1$
			event.doit = false;
			if ("eclipse+external".equals(scheme)) { //$NON-NLS-1$
				final var style = IWorkbenchBrowserSupport.AS_EXTERNAL;
				final var browserSupport = PlatformUI.getWorkbench().getBrowserSupport();
				IWebBrowser browser;
				try {
					browser = browserSupport.createBrowser(style, getPartName(), "", ""); //$NON-NLS-1$ //$NON-NLS-2$
					browser.openURL(resolveEclipseURI(location).toURL());
				} catch (Exception ex) {
					SARLEclipsePlugin.getDefault().logWarningMessage("Unable to open the external browser: " + ex.getLocalizedMessage(), ex); //$NON-NLS-1$
				}
			} else if ("eclipse+command".equals(scheme)) { //$NON-NLS-1$
				final var command = location.getSchemeSpecificPart();
				if ("close".equals(command)) { //$NON-NLS-1$
					hide();
				} if ("dismiss-installation".equals(command)) { //$NON-NLS-1$
					if (this.notification != null) {
						Notifications.setIgnoredNotification(this.notification.getID(), true);
					}
					hide();
				}
			}
		}
	}

}
