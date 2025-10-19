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

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.nio.channels.Channels;
import java.text.MessageFormat;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.OperatingSystem;
import org.arakhne.afc.vmutil.URISchemeType;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ecf.core.ContainerFactory;
import org.eclipse.ecf.core.IContainer;
import org.eclipse.ecf.filetransfer.IFileTransferListener;
import org.eclipse.ecf.filetransfer.IRetrieveFileTransferContainerAdapter;
import org.eclipse.ecf.filetransfer.events.IFileTransferEvent;
import org.eclipse.ecf.filetransfer.events.IIncomingFileTransferReceiveDoneEvent;
import org.eclipse.ecf.filetransfer.events.IIncomingFileTransferReceiveStartEvent;
import org.eclipse.ecf.filetransfer.identity.FileIDFactory;
import org.osgi.framework.Version;

import io.sarl.lang.util.Utils;

/**
 * Tools for notification event checkers. 
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 1.0
 */
public final class SARLCheckers {

	/** Magic number for Windows: 64Mb - 32Kb.
	 */
	private static final long WINDOWS_MAX_FILE_COUNT = (64 * 1024 * 1024) - (32 * 1024);

	private static final String PROVIDER_ID = "ecf.base"; //$NON-NLS-1$

	private static final int DOWNLOAD_TIMEOUT = 60 * 1000;

	private SARLCheckers() {
		//
	}

	/** Perform the check of the update and add a notification in {@link Notifications}.
	 * This function is ran asynchronously to the calling function.
	 * This function creates an Eclipse job.
	 *
	 * @param name the name of the asynchronous task.
	 * @param started a boolean flag that enables to avoid duplicate runs of the checker.
	 * @param task the task to be run.
	 */
	public static void asynchronousCheck(String name, AtomicBoolean started, Consumer<IProgressMonitor> task) {
		final var mirrorJob = new Job(name) {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				if (!started.getAndSet(true)) {
					try {
						if (task != null) {
							task.accept(monitor);
						}
					} finally {
						started.set(false);
					}
				}
				return Status.OK_STATUS;
			}
		};
		mirrorJob.schedule();
	}

	/** Download the string content of the page with the given URL.
	 *
	 * @param url the URL of the page to download.
	 * @param monitor the progress monitor to be used.
	 * @return the string content.
	 * @throws Exception if the content cannot be obtained.
	 */
	public static String downloadString(String url, IProgressMonitor monitor) throws Exception {
		return downloadString(new URI(url), monitor);
	}

	/** Download the string content of the page with the given URL.
	 *
	 * @param uri the URI of the page to download.
	 * @param monitor the progress monitor to be used.
	 * @return the string content.
	 * @throws Exception if the content cannot be obtained.
	 */
	public static String downloadString(URI uri, IProgressMonitor monitor) throws Exception {
		return downloadString(uri.toURL(), monitor);
	}

	/** Download the string content of the page with the given URL.
	 *
	 * @param url the URL of the page to download.
	 * @param monitor the progress monitor to be used.
	 * @return the string content.
	 * @throws Exception if the content cannot be obtained.
	 */
	public static String downloadString(URL url, IProgressMonitor monitor) throws Exception {
		final var array = downloadBytes(url, monitor);
		if (array != null) {
			return new String(array);
		}
		return ""; //$NON-NLS-1$
	}

	private static byte[] transfertFromLocalFile(URL url) throws IOException {
		final var file = FileSystem.convertURLToFile(url);
		try (final var in = new FileInputStream(file)) {
			try (final var inChannel = in.getChannel()) {
				final var bos = new ByteArrayOutputStream();
				try (var outChannel = Channels.newChannel(bos)) {
					final var inSize = inChannel.size();
					if (inSize < 0 || OperatingSystem.WIN.isCurrentOS()) {
						long position = 0;
						long copied = 1;
						while ((inSize >= 0 && position < inSize) || (inSize < 0 && copied > 0)) {
							copied = inChannel.transferTo(position, WINDOWS_MAX_FILE_COUNT, outChannel);
							position += copied;
						}
					} else {
						inChannel.transferTo(0, inSize, outChannel);
					}
				} finally {
					bos.flush();
					bos.close();
				}
				return bos.toByteArray();
			}
		}
	}

	private static byte[] transfertFromRemoteSource(URL url) throws Exception {
		IRetrieveFileTransferContainerAdapter transferService = null;
		IContainer container = null;
		try {
			container = ContainerFactory.getDefault().createContainer(PROVIDER_ID);
			if (container != null) {
				transferService = container.getAdapter(IRetrieveFileTransferContainerAdapter.class);
			}
		} catch (Throwable ex) {
			transferService = null;
		}

		if (transferService == null) {
			throw new IOException(Messages.SARLCheckers_0);
		}

		// Define a listener to handle the incoming file transfer events.
		final var transferListener = new FileTransferListener();
		final var id = FileIDFactory.getDefault().createFileID(transferService.getRetrieveNamespace(), url);

		// Start the transfer.
		transferService.sendRetrieveRequest(id, transferListener, null);

		try {
			final var lock = transferListener.getLock();
			synchronized (lock) {
				lock.wait(DOWNLOAD_TIMEOUT);
			}
		} catch (Throwable ex) {
			throw new Exception(ex);
		}
		
		if (container != null) {
			container.dispose();
		}
		
		if (!transferListener.isSuccess()) {
			final var rawError = transferListener.getError();
			if (rawError instanceof IOException exc) {
				throw exc;
			} else if (rawError != null) {
				throw new IOException(rawError);
			}
			return null;
		}
		return transferListener.getByteArray();
	}

	/** Download the string content of the page with the given URL.
	 *
	 * @param url the URL of the page to download.
	 * @param monitor the progress monitor to be used.
	 * @return the string content.
	 * @throws Exception if the content cannot be obtained.
	 */
	public static byte[] downloadBytes(URL url, IProgressMonitor monitor) throws Exception {
		if (monitor != null) {
			monitor.beginTask(MessageFormat.format(Messages.SARLCheckers_2, url), 1);
		}
		try {
			if (URISchemeType.FILE.isURL(url)) {
				return transfertFromLocalFile(url);
			}
			return transfertFromRemoteSource(url);
		} finally {
			if (monitor != null) {
				monitor.done();
			}
		}
	}

	/** Parse a version number from the string representation of the given argument.
	 *
	 * @param candidate the object to parse.
	 * @return the version number, or {@code null} if a version number cannot be parse.
	 * @see Utils#parseVersion(String)
	 */
	public static Version parseVersion(Object candidate) {
		if (candidate != null) {
			return Utils.parseVersion(candidate.toString());
		}
		return null;
	}

	/**
	 * Listener on the receiving of data from the remote file. 
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 1.0
	 */
	private static class FileTransferListener implements IFileTransferListener {

		private final ByteArrayOutputStream bos = new ByteArrayOutputStream();

		private AtomicReference<Throwable> error = new AtomicReference<>();

		private AtomicBoolean success = new AtomicBoolean();

		private final Object lock = new Object();

		/** Constructor.
		 */
		FileTransferListener() {
			//
		}

		/** Replies the lock.
		 *
		 * @return the lock.
		 */
		public Object getLock() {
			return this.lock;
		}

		/** Replies the received data.
		 *
		 * @return the byte array that is received.
		 * @throws IOException if the bye array cannot be retrieved.
		 */
		public byte[] getByteArray() throws IOException {
			this.bos.flush();
			this.bos.close();
			return this.bos.toByteArray();
		}

		/** Replies the error that was encountered during the file transfer.
		 *
		 * @return the error, or {@code null} if no error was encountered.
		 */
		public Throwable getError() {
			return this.error.get();
		}

		/** Replies if the transfer was a success.
		 *
		 * @return {@code true} on success
		 */
		public boolean isSuccess() {
			return this.success.get();
		}

		@Override
		public void handleTransferEvent(final IFileTransferEvent event) {
			if (event instanceof IIncomingFileTransferReceiveStartEvent startEvent) {
				try {
					startEvent.receive(this.bos);
				} catch (IOException exception) {
					this.error.set(exception);
					this.success.set(false);
					startEvent.cancel();
					this.lock.notifyAll();
				}
			} else if (event instanceof IIncomingFileTransferReceiveDoneEvent doneEvent) {
				final var err = doneEvent.getException();
				this.error.set(err);
				this.success.set(err == null);
				try {
					synchronized (this.lock) {
						this.lock.notifyAll();
					}
				} catch (Throwable ex) {
					//
				}
			}
		}
	}

}
