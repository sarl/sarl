/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.janusproject.kernel.services.zeromq;

import java.io.EOFException;
import java.io.IOException;
import java.net.URI;
import java.nio.ByteBuffer;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;

import com.google.common.primitives.Ints;
import com.google.common.util.concurrent.Service;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.google.inject.name.Named;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;

import io.janusproject.JanusConfig;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.contextspace.SpaceRepositoryListener;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryServiceListener;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.network.AbstractNetworkingExecutionThreadService;
import io.janusproject.services.network.EventDispatch;
import io.janusproject.services.network.EventEnvelope;
import io.janusproject.services.network.EventSerializer;
import io.janusproject.services.network.NetworkServiceListener;

import io.sarl.lang.core.Event;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;

/**
 * Service that is providing the ZeroMQ network.
 *
 * <p>This service is thread-safe.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class ZeroMQNetworkService extends AbstractNetworkingExecutionThreadService {

    private static final long POLL_TIMEOUT = 1000;

    private final Listener serviceListener = new Listener();

    @Inject
    private LogService logger;

    @Inject
    private KernelDiscoveryService kernelService;

    private ContextSpaceService spaceService;

    @Inject
    private ExecutorService executorService;

    @Inject
    private EventSerializer serializer;

    private ZContext context;

    private Socket sendingSocket;

    private Map<URI, Socket> receptionSocketsPerRemoteKernel = new ConcurrentHashMap<>();

    private final Map<SpaceID, NetworkEventReceivingListener> messageRecvListeners = new TreeMap<>();

    // Change poller that can be stopped properly.
    private Poller poller;

    private URI uriCandidate;

    private URI validatedURI;

    private Map<SpaceID, BufferedConnection> bufferedConnections = new TreeMap<>();

    private Map<SpaceID, BufferedSpace> bufferedSpaces = new TreeMap<>();

    private final List<NetworkServiceListener> listeners = new ArrayList<>();

    /**
     * Construct a <code>ZeroMQNetwork</code>.
     *
     * @param uri
     *            - injected URI of the PUB socket.
     */
    @Inject
    public ZeroMQNetworkService(@Named(JanusConfig.PUB_URI) URI uri) {
        assert uri != null : "Injected URI must be not null nor empty"; //$NON-NLS-1$
        this.uriCandidate = uri;
    }

    /** Set the reference to the space service.
     *
     * @param service the service.
     */
    @Inject
    public void setSpaceService(ContextSpaceService service) {
    	if (this.spaceService != null) {
    		this.spaceService.removeSpaceRepositoryListener(this.serviceListener);
    	}
    	this.spaceService = service;
    	if (this.spaceService != null) {
    		this.spaceService.addSpaceRepositoryListener(this.serviceListener);
    	}
    }

    @Override
    public Collection<Class<? extends Service>> getServiceDependencies() {
        return Arrays.<Class<? extends Service>>asList(LogService.class, ExecutorService.class);
    }

    @Override
    public Collection<Class<? extends Service>> getServiceWeakDependencies() {
        return Arrays.<Class<? extends Service>>asList(KernelDiscoveryService.class);
    }

    @Override
    public URI getURI() {
        synchronized (this) {
            return this.validatedURI;
        }
    }

    @Override
    public void addNetworkServiceListener(NetworkServiceListener listener) {
        synchronized (this.listeners) {
            this.listeners.add(listener);
        }
    }

    @Override
    public void removeNetworkServiceListener(NetworkServiceListener listener) {
        synchronized (this.listeners) {
            this.listeners.remove(listener);
        }
    }

    /**
     * Notifies that a peer space was connected.
     *
     * @param peerURI
     *            - the URI of the peer that was connected to.
     * @param space
     *            - the identifier of the connected space.
     */
    protected void firePeerConnected(URI peerURI, SpaceID space) {
        final NetworkServiceListener[] ilisteners;
        synchronized (this.listeners) {
            ilisteners = new NetworkServiceListener[this.listeners.size()];
            this.listeners.toArray(ilisteners);
        }
        for (final NetworkServiceListener listener : ilisteners) {
            listener.peerConnected(peerURI, space);
        }
    }

    /**
     * Notifies that a peer space was disconnected.
     *
     * @param peerURI
     *            - the URI of the peer that was disconnected to.
     * @param space
     *            - the identifier of the disconnected space.
     */
    protected void firePeerDisconnected(URI peerURI, SpaceID space) {
        final NetworkServiceListener[] ilisteners;
        synchronized (this.listeners) {
            ilisteners = new NetworkServiceListener[this.listeners.size()];
            this.listeners.toArray(ilisteners);
        }
        for (final NetworkServiceListener listener : ilisteners) {
            listener.peerDisconnected(peerURI, space);
        }
    }

    /**
     * Notifies that a peer was disconnected.
     *
     * @param peerURI
     *            - the URI of the peer that was disconnected to.
     */
    protected void firePeerDisconnected(URI peerURI) {
        final NetworkServiceListener[] ilisteners;
        synchronized (this.listeners) {
            ilisteners = new NetworkServiceListener[this.listeners.size()];
            this.listeners.toArray(ilisteners);
        }
        for (final NetworkServiceListener listener : ilisteners) {
            listener.peerDisconnected(peerURI);
        }
    }

    /**
     * Notifies that a peer was discovered.
     *
     * @param peerURI
     *            - the URI of the remote kernel that was disconnected to.
     */
    protected void firePeerDiscovered(URI peerURI) {
        final NetworkServiceListener[] ilisteners;
        synchronized (this.listeners) {
            ilisteners = new NetworkServiceListener[this.listeners.size()];
            this.listeners.toArray(ilisteners);
        }
        for (final NetworkServiceListener listener : ilisteners) {
            listener.peerDiscovered(peerURI);
        }
    }

    private void send(EventEnvelope envelope) {
        this.sendingSocket.sendMore(buildFilterableHeader(envelope.getContextId()));
        this.sendingSocket.sendMore(Ints.toByteArray(envelope.getSpaceId().length));
        this.sendingSocket.sendMore(envelope.getSpaceId());
        this.sendingSocket.sendMore(Ints.toByteArray(envelope.getScope().length));
        this.sendingSocket.sendMore(envelope.getScope());
        this.sendingSocket.sendMore(Ints.toByteArray(envelope.getCustomHeaders().length));
        this.sendingSocket.sendMore(envelope.getCustomHeaders());
        this.sendingSocket.sendMore(Ints.toByteArray(envelope.getBody().length));
        this.sendingSocket.send(envelope.getBody());
    }

    /**
     * Build the byte array that may be used for the ZeroMQ filtering associated with {@link Socket#subscribe(byte[])}. For a given contextID
     * (translated into a byte array with an {@link EventSerializer}), this function must always reply the same sequence of bytes.
     *
     * @param contextID
     *            the identifier of the context.
     * @return the header of the ZeroMQ message that may be used for filtering.
     */
    private static byte[] buildFilterableHeader(byte[] contextID) {
        final byte[] header = new byte[Ints.BYTES + contextID.length];
        final byte[] length = Ints.toByteArray(contextID.length);
        System.arraycopy(length, 0, header, 0, length.length);
        System.arraycopy(contextID, 0, header, length.length, contextID.length);
        return header;
    }

    @Override
    public synchronized void publish(Scope<?> scope, Event data) throws Exception {
        if (this.validatedURI == null) {
            this.logger.getKernelLogger().fine(MessageFormat.format(Messages.ZeroMQNetworkService_0, data.getSource().getSpaceID(), scope, data));
        } else if (!this.receptionSocketsPerRemoteKernel.isEmpty()) {
        	// Send asynchronously
        	this.executorService.execute(() -> {
        		try {
	                final SpaceID spaceID = data.getSource().getSpaceID();
	                final EventEnvelope env = this.serializer.serialize(new EventDispatch(spaceID, data, scope));
	                send(env);
	                this.logger.getKernelLogger().fine(MessageFormat.format(Messages.ZeroMQNetworkService_1, spaceID, data));
        		} catch (Exception exception) {
	                this.logger.getKernelLogger().severe(MessageFormat.format(Messages.ZeroMQNetworkService_17, data, scope, exception));
        		}
        	});
        }
    }

    private static byte[] readBuffer(ByteBuffer buffer, int size) throws IOException {
        if (buffer.remaining() >= size) {
            final byte[] result = new byte[size];
            buffer.get(result);
            return result;
        }
        throw new EOFException();
    }

    private static byte[] readBlock(ByteBuffer buffer) throws IOException {
        final int length = Ints.fromByteArray(readBuffer(buffer, Ints.BYTES));
        return readBuffer(buffer, length);
    }

    /**
     * Receive data from the network.
     *
     * @param socket
     *            - network reader.
     * @return the envelope received over the network.
     * @throws IOException
     *             if the envelope cannot be read from the network.
     */
    private static EventEnvelope extractEnvelope(Socket socket) throws IOException {
        // To-Do: Read the ZeroMQ socket via a NIO wrapper to support large data:
        // indeed the arrays has a maximal size bounded by a native int value, and
        // the real data could be larger than this limit.

        byte[] data = socket.recv(ZMQ.DONTWAIT);
        byte[] cdata;
        int oldSize = 0;
        while (socket.hasReceiveMore()) {
            cdata = socket.recv(ZMQ.DONTWAIT);
            oldSize = data.length;
            data = Arrays.copyOf(data, data.length + cdata.length);
            System.arraycopy(cdata, 0, data, oldSize, cdata.length);
        }

        final ByteBuffer buffer = ByteBuffer.wrap(data);

        final byte[] contextId = readBlock(buffer);
        assert contextId != null && contextId.length > 0;

        final byte[] spaceId = readBlock(buffer);
        assert spaceId != null && spaceId.length > 0;

        final byte[] scope = readBlock(buffer);
        assert scope != null && scope.length > 0;

        final byte[] headers = readBlock(buffer);
        assert headers != null && headers.length > 0;

        final byte[] body = readBlock(buffer);
        assert body != null && body.length > 0;

        return new EventEnvelope(contextId, spaceId, scope, headers, body);
    }

    @SuppressWarnings("resource")
    @Override
    public synchronized void connectToRemoteSpaces(URI peerUri, SpaceID space, NetworkEventReceivingListener listener) throws Exception {
        if (this.validatedURI == null) {
            // Bufferizing the peerURI.
            assert this.bufferedConnections != null;
            this.bufferedConnections.put(space, new BufferedConnection(peerUri, space, listener));
        } else {
            Socket receptionSocket = this.receptionSocketsPerRemoteKernel.get(peerUri);
            if (receptionSocket == null) {
                this.logger.getKernelLogger().fine(MessageFormat.format(Messages.ZeroMQNetworkService_2, peerUri, space));
                receptionSocket = this.context.createSocket(ZMQ.SUB);
                assert receptionSocket != null;
                this.receptionSocketsPerRemoteKernel.put(peerUri, receptionSocket);
                receptionSocket.connect(peerUri.toString());
                this.poller.register(receptionSocket, Poller.POLLIN);
                this.logger.getKernelLogger().fine(MessageFormat.format(Messages.ZeroMQNetworkService_3, peerUri));
            }
            assert receptionSocket != null;
            final NetworkEventReceivingListener old = this.messageRecvListeners.get(space);
            if (old == null) {
                assert listener != null;
                this.messageRecvListeners.put(space, listener);
            }
            final byte[] header = buildFilterableHeader(this.serializer.serializeContextID(space.getContextID()));
            receptionSocket.subscribe(header);
            this.logger.getKernelLogger().fine(MessageFormat.format(Messages.ZeroMQNetworkService_4, peerUri, space));
        }
    }

    @SuppressWarnings("resource")
    @Override
    public synchronized void disconnectFromRemoteSpace(URI peer, SpaceID space) throws Exception {
        final Socket socket = this.receptionSocketsPerRemoteKernel.get(peer);
        if (socket != null) {
            this.logger.getKernelLogger().fine(MessageFormat.format(Messages.ZeroMQNetworkService_5, peer, space));
            final byte[] header = buildFilterableHeader(this.serializer.serializeContextID(space.getContextID()));
            socket.unsubscribe(header);
        }
    }

    @SuppressWarnings("resource")
    @Override
    public synchronized void disconnectPeer(URI peer) throws Exception {
        final Socket socket = this.receptionSocketsPerRemoteKernel.remove(peer);
        if (socket != null) {
            this.logger.getKernelLogger().fine(MessageFormat.format(Messages.ZeroMQNetworkService_6, peer));
            this.poller.unregister(socket);
            socket.close();
            this.logger.getKernelLogger().fine(MessageFormat.format(Messages.ZeroMQNetworkService_7, peer));
        }
    }

    /**
     * Extract data from a received envelope, and forwad it to the rest of the platform.
     *
     * @param env
     *            - the evenlope received over the network, and that must be deserialize.
     * @throws Exception
     *             - if cannot deserialize the envelope.
     */
    protected synchronized void receive(EventEnvelope env) throws Exception {
        this.logger.getKernelLogger().fine(MessageFormat.format(Messages.ZeroMQNetworkService_8, this.validatedURI, env));
        final EventDispatch dispatch = this.serializer.deserialize(env);
        this.logger.getKernelLogger().fine(MessageFormat.format(Messages.ZeroMQNetworkService_9, dispatch));

        final SpaceID spaceID = dispatch.getSpaceID();
        final NetworkEventReceivingListener space = this.messageRecvListeners.get(spaceID);
        if (space != null) {
            this.executorService.submit(new AsyncRunner(space, spaceID, dispatch.getScope(), dispatch.getEvent()));
        } else {
            this.logger.getKernelLogger().fine(MessageFormat.format(Messages.ZeroMQNetworkService_10, spaceID, dispatch.getEvent()));
        }
    }

    @Override
    protected void run() throws Exception {
        while (isRunning()) {
            try {
                if (this.poller.getSize() > 0) {
                    final int signaled = this.poller.poll(POLL_TIMEOUT);
                    if (signaled > 0) {
                        for (int i = 0; i < this.poller.getSize(); i++) {
                            if (this.poller.pollin(i)) {
                                this.logger.getKernelLogger().fine(MessageFormat.format(Messages.ZeroMQNetworkService_11, new Integer(i)));
                                final EventEnvelope ev = extractEnvelope(this.poller.getSocket(i));
                                assert ev != null;

                                try {
                                    receive(ev);
                                } catch (Throwable e) {
                                    this.logger.getKernelLogger().fine(MessageFormat.format(Messages.ZeroMQNetworkService_12, e));
                                }
                            } else if (this.poller.pollerr(i)) {
                                final int poolerIdx = i;
                                this.logger.getKernelLogger().warning(() -> {
                                    return ZeroMQNetworkService.this.poller.getSocket(poolerIdx).toString();
                                });
                            }
                        }
                    }
                }
            } catch (Throwable e) {
                this.logger.getKernelLogger().severe(MessageFormat.format(Messages.ZeroMQNetworkService_14, e));
            }
            // ensure that this thread does not take too much time.
            Thread.yield();
        }
        // May the poller be stopped?
        // stopPoller();
    }

    @Override
    protected void startUp() throws Exception {
        final Map<SpaceID, BufferedConnection> connections;
        synchronized (this) {
            super.startUp();
            this.context = new ZContext();
            this.sendingSocket = this.context.createSocket(ZMQ.PUB);
            final String strUri = this.uriCandidate.toString();
            if (this.uriCandidate.getPort() == -1) {
                // Useful when the user do not manually specify a port
                final int port = this.sendingSocket.bindToRandomPort(strUri);
                if (port != -1 && this.uriCandidate.getPort() == -1) {
                    this.validatedURI = new URI(this.uriCandidate.getScheme(), this.uriCandidate.getUserInfo(), this.uriCandidate.getHost(), port,
                            this.uriCandidate.getPath(), this.uriCandidate.getQuery(), this.uriCandidate.getFragment());
                } else {
                    this.validatedURI = this.uriCandidate;
                }
            } else {
                // Useful when the user manually specifies the PUB_URI with -Dnetwork.pub.uri=tcp://XX.XX.XX.XX:port at startup, in this case we do
                // not let ZeroMQ randomly assigns a port but it must use the specified one
                this.sendingSocket.bind(strUri);
                this.validatedURI = this.uriCandidate;
            }

            System.setProperty(JanusConfig.PUB_URI, this.validatedURI.toString());
            this.logger.getKernelLogger().fine(MessageFormat.format(Messages.ZeroMQNetworkService_15, this.validatedURI));
            this.uriCandidate = null;
            connections = this.bufferedConnections;
            this.bufferedConnections = null;
            this.poller = this.context.createPoller(1);

            this.kernelService.addKernelDiscoveryServiceListener(this.serviceListener);
        }
        for (final BufferedConnection t : connections.values()) {
            connectToRemoteSpaces(t.getPeerURI(), t.getSpaceID(), t.getListener());
        }
    }

    @Override
    protected void shutDown() throws Exception {
        synchronized (this) {
            this.kernelService.removeKernelDiscoveryServiceListener(this.serviceListener);
            this.spaceService.removeSpaceRepositoryListener(this.serviceListener);

            // this.poller.stop();
            // stopPoller();

            // this.publisher.close();

            this.context.destroy();
        }
        this.logger.getKernelLogger().finer(Messages.ZeroMQNetworkService_16);
    }

    /**
     * Connection that is buffering messages.
     *
     * @author $Author: sgalland$
     * @version $FullVersion$
     * @mavengroupid $GroupId$
     * @mavenartifactid $ArtifactId$
     */
    private static class BufferedConnection {

        /**
         * URI of the peer.
         */
        private final URI peerURI;

        /**
         * ID of the space.
         */
        private final SpaceID spaceID;

        /**
         * Reception listener.
         */
        private final NetworkEventReceivingListener listener;

        /**
         * Construct.
         * @param peerURI
         *            the URI of the peer.
         * @param spaceID
         *            the identifier of the space.
         * @param listener
         *            the network event listener.
         */
        BufferedConnection(URI peerURI, SpaceID spaceID, NetworkEventReceivingListener listener) {
            this.peerURI = peerURI;
            this.spaceID = spaceID;
            this.listener = listener;
        }

        public URI getPeerURI() {
            return this.peerURI;
        }

        public SpaceID getSpaceID() {
            return this.spaceID;
        }

        public NetworkEventReceivingListener getListener() {
            return this.listener;
        }

    }

    /**
     * Spacec descritpion that is bufferred.
     *
     * @author $Author: sgalland$
     * @version $FullVersion$
     * @mavengroupid $GroupId$
     * @mavenartifactid $ArtifactId$
     */
    private static class BufferedSpace {

        /**
         * ID of the space.
         */
        private final SpaceID spaceID;

        /**
         * Reception listener.
         */
        private final NetworkEventReceivingListener listener;

        /**
         * Construct.
         *
         * @param spaceID
         *            the space identifier.
         * @param listener
         *            the network event listener.
         */
        BufferedSpace(SpaceID spaceID, NetworkEventReceivingListener listener) {
            this.spaceID = spaceID;
            this.listener = listener;
        }

        public SpaceID getSpaceID() {
            return this.spaceID;
        }

        public NetworkEventReceivingListener getListener() {
            return this.listener;
        }

    }

    /**
     * Asynchronous runner.
     *
     * @author $Author: sgalland$
     * @version $FullVersion$
     * @mavengroupid $GroupId$
     * @mavenartifactid $ArtifactId$
     */
    private class AsyncRunner implements Runnable {

        private final NetworkEventReceivingListener space;

        private final SpaceID spaceID;

        private final Scope<?> scope;

        private final Event event;

        AsyncRunner(NetworkEventReceivingListener space, SpaceID spaceID, Scope<?> scope, Event event) {
            this.space = space;
            this.spaceID = spaceID;
            this.scope = scope;
            this.event = event;
        }

        @Override
        public void run() {
            this.space.eventReceived(this.spaceID, this.scope, this.event);
        }
    }

    /**
     * Listener on platform events for updating the ZeroMQ service.
     *
     * @author $Author: sgalland$
     * @version $FullVersion$
     * @mavengroupid $GroupId$
     * @mavenartifactid $ArtifactId$
     */
    private class Listener implements SpaceRepositoryListener, KernelDiscoveryServiceListener {

        /**
         * Construct.
         */
        Listener() {
            //
        }

        @SuppressWarnings("synthetic-access")
        private void magicConnect(URI peer, Collection<SpaceID> spaceIDs, Collection<BufferedSpace> ibufferedSpaces, Space space) {
            if (space != null) {
                try {
                    connectToRemoteSpaces(peer, space.getSpaceID(), (NetworkEventReceivingListener) space);
                } catch (Exception e) {
                    ZeroMQNetworkService.this.logger.getKernelLogger().severe(MessageFormat.format(Messages.ZeroMQNetworkService_14, e));
                }
            }
            for (final SpaceID sid : spaceIDs) {
                try {
                    // Below, the null constant does not change the SPACEID->LISTENER map
                    connectToRemoteSpaces(peer, sid, null);
                } catch (Exception e) {
                    ZeroMQNetworkService.this.logger.getKernelLogger().severe(MessageFormat.format(Messages.ZeroMQNetworkService_14, e));
                }
            }
            for (final BufferedSpace sp : ibufferedSpaces) {
                try {
                    connectToRemoteSpaces(peer, sp.getSpaceID(), sp.getListener());
                } catch (Exception e) {
                    ZeroMQNetworkService.this.logger.getKernelLogger().severe(MessageFormat.format(Messages.ZeroMQNetworkService_14, e));
                }
            }
        }

        @SuppressWarnings("synthetic-access")
        @Override
        public void spaceCreated(Space space, boolean isLocalCreation) {
            synchronized (ZeroMQNetworkService.this) {
                final URI localUri = ZeroMQNetworkService.this.getURI();
                try {
                    boolean isUsed = false;
                    final Collection<SpaceID> spaceIDs = new ArrayList<>(ZeroMQNetworkService.this.messageRecvListeners.keySet());
                    final Collection<BufferedSpace> spaces = new ArrayList<>(ZeroMQNetworkService.this.bufferedSpaces.values());
                    for (final URI peer : ZeroMQNetworkService.this.kernelService.getKernels()) {
                        if (!peer.equals(localUri)) {
                            if (space instanceof NetworkEventReceivingListener) {
                                magicConnect(peer, spaceIDs, spaces, space);
                                isUsed = true;
                            } else {
                                ZeroMQNetworkService.this.logger.getKernelLogger().severe(MessageFormat.format(Messages.ZeroMQNetworkService_20,
                                        space));
                            }
                        }
                    }
                    if (!isUsed) {
                        // The space was not used to be connected to a remote host => put in a buffer.
                        if (space instanceof NetworkEventReceivingListener) {
                            ZeroMQNetworkService.this.bufferedSpaces.put(space.getSpaceID(),
                                    new BufferedSpace(space.getSpaceID(), (NetworkEventReceivingListener) space));
                        } else {
                            ZeroMQNetworkService.this.logger.getKernelLogger().severe(MessageFormat.format(
                            		Messages.ZeroMQNetworkService_20, space));
                        }
                    } else {
                        // The buffer was consumed by the "magicConnect"
                        ZeroMQNetworkService.this.bufferedSpaces.clear();
                    }
                } catch (Exception e) {
                    ZeroMQNetworkService.this.logger.getKernelLogger().severe(MessageFormat.format(Messages.ZeroMQNetworkService_14, e));
                }
            }
        }

        @SuppressWarnings("synthetic-access")
        @Override
        public void spaceDestroyed(Space space, boolean isLocalDestruction) {
            synchronized (ZeroMQNetworkService.this) {
                final URI localUri = ZeroMQNetworkService.this.getURI();
                try {
                    for (final URI peer : ZeroMQNetworkService.this.kernelService.getKernels()) {
                        if (!peer.equals(localUri)) {
                            disconnectFromRemoteSpace(peer, space.getSpaceID());
                        }
                    }
                    // Ensure that the space becomes unknown
                    ZeroMQNetworkService.this.messageRecvListeners.remove(space.getSpaceID());
                    if (ZeroMQNetworkService.this.bufferedConnections != null) {
                        ZeroMQNetworkService.this.bufferedConnections.remove(space.getSpaceID());
                    }
                    ZeroMQNetworkService.this.bufferedSpaces.remove(space.getSpaceID());
                } catch (Exception e) {
                    ZeroMQNetworkService.this.logger.getKernelLogger().severe(MessageFormat.format(Messages.ZeroMQNetworkService_14, e));
                }
            }
        }

        @SuppressWarnings("synthetic-access")
        @Override
        public void kernelDiscovered(URI peerURI) {
            synchronized (ZeroMQNetworkService.this) {
                final URI localUri = ZeroMQNetworkService.this.getURI();
                final Collection<SpaceID> spaceIDs = new ArrayList<>(ZeroMQNetworkService.this.messageRecvListeners.keySet());
                final Collection<BufferedSpace> spaces = new ArrayList<>(ZeroMQNetworkService.this.bufferedSpaces.values());
                if (!spaceIDs.isEmpty() || !spaces.isEmpty()) {
                    boolean cleanBuffers = false;
                    for (final URI peer : ZeroMQNetworkService.this.kernelService.getKernels()) {
                        if (!peer.equals(localUri)) {
                            magicConnect(peer, spaceIDs, spaces, null);
                            cleanBuffers = true;
                        }
                    }
                    if (cleanBuffers) {
                        ZeroMQNetworkService.this.bufferedSpaces.clear();
                    }
                }
            }
        }

        @SuppressWarnings("synthetic-access")
        @Override
        public void kernelDisconnected(URI peerURI) {
            synchronized (ZeroMQNetworkService.this) {
                try {
                    final URI localUri = ZeroMQNetworkService.this.getURI();
                    if (!peerURI.equals(localUri)) {
                        disconnectPeer(peerURI);
                    }
                } catch (Exception e) {
                    ZeroMQNetworkService.this.logger.getKernelLogger().severe(MessageFormat.format(Messages.ZeroMQNetworkService_14, e));
                }
            }
        }

    }

}
