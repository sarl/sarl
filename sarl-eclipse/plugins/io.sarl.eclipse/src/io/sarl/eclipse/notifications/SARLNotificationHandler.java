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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.databinding.observable.list.IListChangeListener;
import org.eclipse.core.databinding.observable.list.ListChangeEvent;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.util.UIUtils;

/**
 * A command handler for the SARL notifications.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 1.0
 */
public class SARLNotificationHandler extends AbstractHandler {

	/** Identifier of the handler in the plugin definition.
	 */
	public static final String HANDLER_ID = "io.sarl.eclipse.notifications.Show"; //$NON-NLS-1$
	
	@Override
	public void setEnabled(Object evaluationContext) {
		final var notifications = Notifications.getNotifications();
		setBaseEnabled(!notifications.isEmpty());
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		final var notifications = Notifications.getNotifications();
		try {
			var count = 0;
			for (final var notification : notifications) {
				++count;
				if (!Notifications.isIgnoredNotification(notification.getID()) && !notification.markAsUsed()) {
					if (notification.isStandardViewSupport()) {
						final var notificationView = createView(count, notification);
						if (count == 1) {
							notificationView.getSite().getPage().bringToTop(notificationView);
						}
						notificationView.setNotification(notification);
					} else {
						final var view = notification.showNotificationUI();
						if (view != null) {
							view.apply();
						}
					}
				}
			}
		} catch (Exception ex) {
			SARLEclipsePlugin.getDefault().logWarningMessage(ex);
		}
		return SARLEclipsePlugin.getDefault().createOkStatus();
	}

	/** Open a view for the given notification.
	 *
	 * @param count the index of the notification in the list of notifications
	 * @param notification the notification.
	 * @return the opened view.
	 * @throws Exception if the view cannot be created.
	 */
	@SuppressWarnings("static-method")
	protected SARLNotificationViewPart createView(int count, Notification notification) throws Exception {
		return (SARLNotificationViewPart) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView(
				SARLNotificationViewPart.VIEW_ID, "ID" + count, //$NON-NLS-1$
				IWorkbenchPage.VIEW_VISIBLE);
	}

	/** Register this handler on changes in the notification list.
	 */
	public static void register() {
		Notifications.addChangeListener(new RunOnChange());
	}

	/**
	 * Listener on notification list changes that causes the launch of {@link SARLNotificationHandler}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 1.0
	 */
	public static class RunOnChange implements IListChangeListener<Notification> {

		@Override
		public void handleListChange(ListChangeEvent<? extends Notification> event) {
			UIUtils.startCommand(HANDLER_ID);
		}

	}

}
