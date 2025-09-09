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

package io.sarl.eclipse.log;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.ref.WeakReference;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.Base64;
import java.util.Objects;

import com.google.common.io.Files;
import com.google.gson.GsonBuilder;
import org.eclipse.core.net.proxy.IProxyService;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.xtext.util.Strings;
import org.osgi.util.tracker.ServiceTracker;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;

/**
 * Wizard for submiting an issue to the SARL community.
 *
 * @author $Author: sgalland$
 * @version io.sarl.eclipse 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @since 0.5
 */
public class SubmitEclipseLogWizard extends Wizard {

	private static final String GITHUB_PUBLIC_URL = "https://github.com/sarl/sarl/issues/new"; //$NON-NLS-1$

	private static final String GITHUB_URL = "https://api.github.com/repos/sarl/sarl/issues"; //$NON-NLS-1$
	//private static final String GITHUB_URL = "https://posttestserver.com/post.php?dump&html"; //$NON-NLS-1$

	// Response code for Github API
	private static final int RESPONSE_CODE = 201;

	private IssueInformationPage detailPage;

	private WeakReference<WizardDialog> wizardDialog;

	/** Constructor.
	 */
	public SubmitEclipseLogWizard() {
		setDefaultPageImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(
				SARLEclipseConfig.SUBMIT_ISSUE_WIZARD_DIALOG_IMAGE));
		setWindowTitle(Messages.SubmitEclipseLogWizard_0);
	}

	/** Open the wizard.
	 *
	 * <p>This method waits until the window is closed by the end user, and then it returns the window's return code;
	 * otherwise, this method returns immediately. A window's return codes are
	 * window-specific, although two standard return codes are predefined:
	 * {@code OK} and {@code CANCEL}.
	 * </p>
	 *
	 * @param parentShell the parent shell.
	 * @return the return code.
	 */
	public static int open(Shell parentShell) {
		final var wizard = new SubmitEclipseLogWizard();
		final var dialog = new WizardDialog(parentShell, wizard);
		wizard.setWizardDialog(dialog);
		return dialog.open();
	}

	/** Change the associated wieard dialog.
	 *
	 * @param dialog the dialog.
	 */
	void setWizardDialog(WizardDialog dialog) {
		assert dialog != null;
		this.wizardDialog = new WeakReference<>(dialog);
	}

	/** Replies the associated wieard dialog.
	 *
	 * @return the dialog.
	 */
	WizardDialog getWizardDialog() {
		final var ref = this.wizardDialog;
		return (ref == null) ? null : ref.get();
	}

	@Override
	public void addPages() {
		URL url = null;
		try {
			url = new URI(GITHUB_PUBLIC_URL).toURL();
		} catch (Exception exception) {
			//
		}
		this.detailPage = new IssueInformationPage(url);
		addPage(this.detailPage);
	}

	@Override
	public boolean performFinish() {
		if (this.detailPage.performFinish()) {
			try {
				final var title = this.detailPage.getIssueTitle();
				final var description = this.detailPage.getIssueDescription();
				final var login = this.detailPage.getGithubLogin();
				final var password = this.detailPage.getGithubPassword();
				final var job = Job.create(Messages.SubmitEclipseLogWizard_0, monitor -> {
					try {
						final var subMonitor = SubMonitor.convert(monitor, 2);
						subMonitor.setTaskName(Messages.SubmitEclipseLogWizard_1);
						final var charset = Charset.defaultCharset();
						final var content = buildContent(description, charset);
						subMonitor.setWorkRemaining(1);
						if (subMonitor.isCanceled()) {
							return Status.CANCEL_STATUS;
						}
						return submit(charset,
								title,
								content,
								login,
								password,
								subMonitor.split(1));
					} catch (Exception exception) {
						return SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception);
					}
				});
				job.schedule();
				return true;
			} catch (Exception e) {
				ErrorDialog.openError(getShell(), e.getLocalizedMessage(), e.getLocalizedMessage(),
						SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, e));
			}
		}
		return false;
	}

	/** Submit the issue to github.
	 *
	 * @param charset the encoding character set.
	 * @param title the issue title.
	 * @param body the issue description.
	 * @param login the Github login.
	 * @param password the Github password.
	 * @param progress the progress monitor (never {@code null}).
	 * @return the status.
	 * @throws Exception when error.
	 */
	@SuppressWarnings("static-method")
	protected IStatus submit(Charset charset, String title, String body, String login, String password,
			IProgressMonitor progress) throws Exception {
		final var subMonitor = SubMonitor.convert(progress, 10);

		subMonitor.setTaskName(Messages.SubmitEclipseLogWizard_15);
		final var gson = new GsonBuilder().create();
		final var json = gson.toJson(new GithubIssueJson(title, body));
		subMonitor.setWorkRemaining(9);

		subMonitor.setTaskName(Messages.SubmitEclipseLogWizard_2);
		final var proxyTracker = new ServiceTracker<IProxyService, IProxyService>(
				SARLEclipsePlugin.getDefault().getBundle().getBundleContext(),
				IProxyService.class,
				null);
		proxyTracker.open();
		if (subMonitor.isCanceled()) {
			return Status.CANCEL_STATUS;
		}
		final URL url;
		try {
			final URI uri = new URI(GITHUB_URL);

			final var proxyDataForHost = proxyTracker.getService().select(uri);
			for (final var data : proxyDataForHost) {
				if (data.getHost() != null) {
					System.setProperty("http.proxySet", "true"); //$NON-NLS-1$ //$NON-NLS-2$
					System.setProperty("http.proxyHost", data.getHost()); //$NON-NLS-1$
				}
				if (data.getHost() != null) {
					System.setProperty("http.proxyPort", String.valueOf(data //$NON-NLS-1$
							.getPort()));
				}
				if (subMonitor.isCanceled()) {
					return Status.CANCEL_STATUS;
				}
			}

			url = uri.toURL();
		} finally {
			proxyTracker.close();
		}
		subMonitor.setWorkRemaining(8);
		if (subMonitor.isCanceled()) {
			return Status.CANCEL_STATUS;
		}

		subMonitor.setTaskName(Messages.SubmitEclipseLogWizard_3);
		final var con = (HttpURLConnection) url.openConnection();
		subMonitor.setWorkRemaining(7);
		if (subMonitor.isCanceled()) {
			return Status.CANCEL_STATUS;
		}

		//add request header
		subMonitor.setTaskName(Messages.SubmitEclipseLogWizard_4);
		con.setRequestMethod("POST"); //$NON-NLS-1$
		con.setRequestProperty("User-Agent", "SARL IDE"); //$NON-NLS-1$ //$NON-NLS-2$
		// Auth
		final var auth = Base64.getEncoder().encodeToString((login + ":" + password).getBytes()); //$NON-NLS-1$
		con.setRequestProperty("Authorization", "Basic " + auth); //$NON-NLS-1$ //$NON-NLS-2$

		con.setDoOutput(true);
		con.setDoInput(true);
		subMonitor.setWorkRemaining(6);
		if (subMonitor.isCanceled()) {
			return Status.CANCEL_STATUS;
		}

		// Send post request
		try (var writer = new DataOutputStream(con.getOutputStream())) {
			writer.writeBytes(json);
			writer.flush();
		}
		subMonitor.setWorkRemaining(3);
		if (subMonitor.isCanceled()) {
			return Status.CANCEL_STATUS;
		}

		//final int responseCode = con.getResponseCode();
		subMonitor.setTaskName(Messages.SubmitEclipseLogWizard_6);
		final var responseCode = con.getResponseCode();
		final var response = new StringBuffer();
		try (var reader = new BufferedReader(new InputStreamReader(con.getInputStream()))) {
			String inputLine;
			while ((inputLine = reader.readLine()) != null) {
				response.append(inputLine);
				if (subMonitor.isCanceled()) {
					return Status.CANCEL_STATUS;
				}
			}
		}
		subMonitor.setWorkRemaining(0);
		if (subMonitor.isCanceled()) {
			return Status.CANCEL_STATUS;
		}

		if (responseCode != RESPONSE_CODE) {
			return SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR,
					Messages.SubmitEclipseLogWizard_14,
					new Exception(response.toString()));
		}

		return Status.OK_STATUS;
	}

	/** Build the issue content.
	 *
	 * @param description the description of the issue.
	 * @param charset the charset to use.
	 * @return the content.
	 * @throws IOException if the content cannot be built.
	 */
	@SuppressWarnings("static-method")
	protected String buildContent(String description, Charset charset) throws IOException {
		final var fullContent = new StringBuilder();

		// User message
		if (!Strings.isEmpty(description)) {
			fullContent.append(description);
			fullContent.append(Messages.SubmitEclipseLogWizard_8);
		}

		// Log
		final var plugin = SARLEclipsePlugin.getDefault();
		plugin.getLog().log(plugin.createStatus(IStatus.INFO, Messages.SubmitEclipseLogWizard_12));

		fullContent.append(Messages.SubmitEclipseLogWizard_9);
		final var filename = Platform.getLogFileLocation().toOSString();
		final var log = new File(filename);
		if (!log.exists()) {
			throw new IOException("Unable to find the log file"); //$NON-NLS-1$
		}
		final var logLines = Files.readLines(log, charset);
		var logStartIndex = -1;
		for (var i = logLines.size() - 1; logStartIndex == -1 && i >= 0; --i) {
			final var line = logLines.get(i);
			if (line.startsWith("!SESSION")) { //$NON-NLS-1$
				logStartIndex = i;
			}
		}
		for (var i = logStartIndex; i < logLines.size(); ++i) {
			fullContent.append(logLines.get(i));
			fullContent.append(Messages.SubmitEclipseLogWizard_8);
		}

		// Properties
		fullContent.append(Messages.SubmitEclipseLogWizard_10);
		for (final var entry : System.getProperties().entrySet()) {
			fullContent.append(Objects.toString(entry.getKey()));
			fullContent.append(Messages.SubmitEclipseLogWizard_11);
			fullContent.append(Objects.toString(entry.getValue()));
			fullContent.append(Messages.SubmitEclipseLogWizard_8);
		}
		fullContent.append(Messages.SubmitEclipseLogWizard_13);

		return fullContent.toString();
	}

	/** Definition of the Json entries for creating a Github issue.
	 *
	 * @author $Author: sgalland$
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 * @since 0.5
	 */
	public static class GithubIssueJson {

		private final String title;

		private final String body;

		/** Constructor.
		 *
		 * @param title the title of the issue.
		 * @param body the description of the issue.
		 */
		public GithubIssueJson(String title, String body) {
			this.title = title;
			this.body = body;
		}

		/** Replies the title of the issue.
		 *
		 * @return the title.
		 */
		public String getTitle() {
			return this.title;
		}

		/** Replies the description of the issue.
		 *
		 * @return the description.
		 */
		public String getBody() {
			return this.body;
		}

	}

}
