/**
 * 
 */
package io.sarl.eclipse.navigator;

import org.eclipse.ui.navigator.CommonNavigator;

/**
 * SARL custom project navigator
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLProjectNavigator extends CommonNavigator {
    @Override
    protected Object getInitialInput() {
        return new SARLProjectWorkbenchRoot();
    }
}
