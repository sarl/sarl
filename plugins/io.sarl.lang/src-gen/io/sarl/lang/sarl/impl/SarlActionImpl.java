/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.xtend.core.xtend.CreateExtensionInfo;
import org.eclipse.xtend.core.xtend.XtendParameter;

import org.eclipse.xtend.core.xtend.impl.XtendMemberImpl;

import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;

import org.eclipse.xtext.xbase.XExpression;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Action</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlActionImpl#getTypeParameters <em>Type Parameters</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlActionImpl#getName <em>Name</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlActionImpl#getParameters <em>Parameters</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlActionImpl#isVarargs <em>Varargs</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlActionImpl#getReturnType <em>Return Type</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlActionImpl#getCreateExtensionInfo <em>Create Extension Info</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlActionImpl#getExceptions <em>Exceptions</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlActionImpl#getFiredEvents <em>Fired Events</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlActionImpl#getExpression <em>Expression</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class SarlActionImpl extends XtendMemberImpl implements SarlAction
{
  /**
   * The cached value of the '{@link #getTypeParameters() <em>Type Parameters</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getTypeParameters()
   * @generated
   * @ordered
   */
  protected EList<JvmTypeParameter> typeParameters;

  /**
   * The default value of the '{@link #getName() <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getName()
   * @generated
   * @ordered
   */
  protected static final String NAME_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getName() <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getName()
   * @generated
   * @ordered
   */
  protected String name = NAME_EDEFAULT;

  /**
   * The cached value of the '{@link #getParameters() <em>Parameters</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getParameters()
   * @generated
   * @ordered
   */
  protected EList<XtendParameter> parameters;

  /**
   * The default value of the '{@link #isVarargs() <em>Varargs</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #isVarargs()
   * @generated
   * @ordered
   */
  protected static final boolean VARARGS_EDEFAULT = false;

  /**
   * The cached value of the '{@link #isVarargs() <em>Varargs</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #isVarargs()
   * @generated
   * @ordered
   */
  protected boolean varargs = VARARGS_EDEFAULT;

  /**
   * The cached value of the '{@link #getReturnType() <em>Return Type</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getReturnType()
   * @generated
   * @ordered
   */
  protected JvmTypeReference returnType;

  /**
   * The cached value of the '{@link #getCreateExtensionInfo() <em>Create Extension Info</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getCreateExtensionInfo()
   * @generated
   * @ordered
   */
  protected CreateExtensionInfo createExtensionInfo;

  /**
   * The cached value of the '{@link #getExceptions() <em>Exceptions</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getExceptions()
   * @generated
   * @ordered
   */
  protected EList<JvmTypeReference> exceptions;

  /**
   * The cached value of the '{@link #getFiredEvents() <em>Fired Events</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getFiredEvents()
   * @generated
   * @ordered
   */
  protected EList<JvmTypeReference> firedEvents;

  /**
   * The cached value of the '{@link #getExpression() <em>Expression</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getExpression()
   * @generated
   * @ordered
   */
  protected XExpression expression;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected SarlActionImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  protected EClass eStaticClass()
  {
    return SarlPackage.Literals.SARL_ACTION;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<JvmTypeParameter> getTypeParameters()
  {
    if (typeParameters == null)
    {
      typeParameters = new EObjectContainmentEList<JvmTypeParameter>(JvmTypeParameter.class, this, SarlPackage.SARL_ACTION__TYPE_PARAMETERS);
    }
    return typeParameters;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getName()
  {
    return name;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setName(String newName)
  {
    String oldName = name;
    name = newName;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_ACTION__NAME, oldName, name));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<XtendParameter> getParameters()
  {
    if (parameters == null)
    {
      parameters = new EObjectContainmentEList<XtendParameter>(XtendParameter.class, this, SarlPackage.SARL_ACTION__PARAMETERS);
    }
    return parameters;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public boolean isVarargs()
  {
    return varargs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setVarargs(boolean newVarargs)
  {
    boolean oldVarargs = varargs;
    varargs = newVarargs;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_ACTION__VARARGS, oldVarargs, varargs));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public JvmTypeReference getReturnType()
  {
    return returnType;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetReturnType(JvmTypeReference newReturnType, NotificationChain msgs)
  {
    JvmTypeReference oldReturnType = returnType;
    returnType = newReturnType;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_ACTION__RETURN_TYPE, oldReturnType, newReturnType);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setReturnType(JvmTypeReference newReturnType)
  {
    if (newReturnType != returnType)
    {
      NotificationChain msgs = null;
      if (returnType != null)
        msgs = ((InternalEObject)returnType).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SarlPackage.SARL_ACTION__RETURN_TYPE, null, msgs);
      if (newReturnType != null)
        msgs = ((InternalEObject)newReturnType).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SarlPackage.SARL_ACTION__RETURN_TYPE, null, msgs);
      msgs = basicSetReturnType(newReturnType, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_ACTION__RETURN_TYPE, newReturnType, newReturnType));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public CreateExtensionInfo getCreateExtensionInfo()
  {
    return createExtensionInfo;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetCreateExtensionInfo(CreateExtensionInfo newCreateExtensionInfo, NotificationChain msgs)
  {
    CreateExtensionInfo oldCreateExtensionInfo = createExtensionInfo;
    createExtensionInfo = newCreateExtensionInfo;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_ACTION__CREATE_EXTENSION_INFO, oldCreateExtensionInfo, newCreateExtensionInfo);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setCreateExtensionInfo(CreateExtensionInfo newCreateExtensionInfo)
  {
    if (newCreateExtensionInfo != createExtensionInfo)
    {
      NotificationChain msgs = null;
      if (createExtensionInfo != null)
        msgs = ((InternalEObject)createExtensionInfo).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SarlPackage.SARL_ACTION__CREATE_EXTENSION_INFO, null, msgs);
      if (newCreateExtensionInfo != null)
        msgs = ((InternalEObject)newCreateExtensionInfo).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SarlPackage.SARL_ACTION__CREATE_EXTENSION_INFO, null, msgs);
      msgs = basicSetCreateExtensionInfo(newCreateExtensionInfo, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_ACTION__CREATE_EXTENSION_INFO, newCreateExtensionInfo, newCreateExtensionInfo));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<JvmTypeReference> getExceptions()
  {
    if (exceptions == null)
    {
      exceptions = new EObjectContainmentEList<JvmTypeReference>(JvmTypeReference.class, this, SarlPackage.SARL_ACTION__EXCEPTIONS);
    }
    return exceptions;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<JvmTypeReference> getFiredEvents()
  {
    if (firedEvents == null)
    {
      firedEvents = new EObjectContainmentEList<JvmTypeReference>(JvmTypeReference.class, this, SarlPackage.SARL_ACTION__FIRED_EVENTS);
    }
    return firedEvents;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public XExpression getExpression()
  {
    return expression;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetExpression(XExpression newExpression, NotificationChain msgs)
  {
    XExpression oldExpression = expression;
    expression = newExpression;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_ACTION__EXPRESSION, oldExpression, newExpression);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setExpression(XExpression newExpression)
  {
    if (newExpression != expression)
    {
      NotificationChain msgs = null;
      if (expression != null)
        msgs = ((InternalEObject)expression).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SarlPackage.SARL_ACTION__EXPRESSION, null, msgs);
      if (newExpression != null)
        msgs = ((InternalEObject)newExpression).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SarlPackage.SARL_ACTION__EXPRESSION, null, msgs);
      msgs = basicSetExpression(newExpression, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.SARL_ACTION__EXPRESSION, newExpression, newExpression));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
  {
    switch (featureID)
    {
      case SarlPackage.SARL_ACTION__TYPE_PARAMETERS:
        return ((InternalEList<?>)getTypeParameters()).basicRemove(otherEnd, msgs);
      case SarlPackage.SARL_ACTION__PARAMETERS:
        return ((InternalEList<?>)getParameters()).basicRemove(otherEnd, msgs);
      case SarlPackage.SARL_ACTION__RETURN_TYPE:
        return basicSetReturnType(null, msgs);
      case SarlPackage.SARL_ACTION__CREATE_EXTENSION_INFO:
        return basicSetCreateExtensionInfo(null, msgs);
      case SarlPackage.SARL_ACTION__EXCEPTIONS:
        return ((InternalEList<?>)getExceptions()).basicRemove(otherEnd, msgs);
      case SarlPackage.SARL_ACTION__FIRED_EVENTS:
        return ((InternalEList<?>)getFiredEvents()).basicRemove(otherEnd, msgs);
      case SarlPackage.SARL_ACTION__EXPRESSION:
        return basicSetExpression(null, msgs);
    }
    return super.eInverseRemove(otherEnd, featureID, msgs);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object eGet(int featureID, boolean resolve, boolean coreType)
  {
    switch (featureID)
    {
      case SarlPackage.SARL_ACTION__TYPE_PARAMETERS:
        return getTypeParameters();
      case SarlPackage.SARL_ACTION__NAME:
        return getName();
      case SarlPackage.SARL_ACTION__PARAMETERS:
        return getParameters();
      case SarlPackage.SARL_ACTION__VARARGS:
        return isVarargs();
      case SarlPackage.SARL_ACTION__RETURN_TYPE:
        return getReturnType();
      case SarlPackage.SARL_ACTION__CREATE_EXTENSION_INFO:
        return getCreateExtensionInfo();
      case SarlPackage.SARL_ACTION__EXCEPTIONS:
        return getExceptions();
      case SarlPackage.SARL_ACTION__FIRED_EVENTS:
        return getFiredEvents();
      case SarlPackage.SARL_ACTION__EXPRESSION:
        return getExpression();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @SuppressWarnings("unchecked")
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SarlPackage.SARL_ACTION__TYPE_PARAMETERS:
        getTypeParameters().clear();
        getTypeParameters().addAll((Collection<? extends JvmTypeParameter>)newValue);
        return;
      case SarlPackage.SARL_ACTION__NAME:
        setName((String)newValue);
        return;
      case SarlPackage.SARL_ACTION__PARAMETERS:
        getParameters().clear();
        getParameters().addAll((Collection<? extends XtendParameter>)newValue);
        return;
      case SarlPackage.SARL_ACTION__VARARGS:
        setVarargs((Boolean)newValue);
        return;
      case SarlPackage.SARL_ACTION__RETURN_TYPE:
        setReturnType((JvmTypeReference)newValue);
        return;
      case SarlPackage.SARL_ACTION__CREATE_EXTENSION_INFO:
        setCreateExtensionInfo((CreateExtensionInfo)newValue);
        return;
      case SarlPackage.SARL_ACTION__EXCEPTIONS:
        getExceptions().clear();
        getExceptions().addAll((Collection<? extends JvmTypeReference>)newValue);
        return;
      case SarlPackage.SARL_ACTION__FIRED_EVENTS:
        getFiredEvents().clear();
        getFiredEvents().addAll((Collection<? extends JvmTypeReference>)newValue);
        return;
      case SarlPackage.SARL_ACTION__EXPRESSION:
        setExpression((XExpression)newValue);
        return;
    }
    super.eSet(featureID, newValue);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eUnset(int featureID)
  {
    switch (featureID)
    {
      case SarlPackage.SARL_ACTION__TYPE_PARAMETERS:
        getTypeParameters().clear();
        return;
      case SarlPackage.SARL_ACTION__NAME:
        setName(NAME_EDEFAULT);
        return;
      case SarlPackage.SARL_ACTION__PARAMETERS:
        getParameters().clear();
        return;
      case SarlPackage.SARL_ACTION__VARARGS:
        setVarargs(VARARGS_EDEFAULT);
        return;
      case SarlPackage.SARL_ACTION__RETURN_TYPE:
        setReturnType((JvmTypeReference)null);
        return;
      case SarlPackage.SARL_ACTION__CREATE_EXTENSION_INFO:
        setCreateExtensionInfo((CreateExtensionInfo)null);
        return;
      case SarlPackage.SARL_ACTION__EXCEPTIONS:
        getExceptions().clear();
        return;
      case SarlPackage.SARL_ACTION__FIRED_EVENTS:
        getFiredEvents().clear();
        return;
      case SarlPackage.SARL_ACTION__EXPRESSION:
        setExpression((XExpression)null);
        return;
    }
    super.eUnset(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public boolean eIsSet(int featureID)
  {
    switch (featureID)
    {
      case SarlPackage.SARL_ACTION__TYPE_PARAMETERS:
        return typeParameters != null && !typeParameters.isEmpty();
      case SarlPackage.SARL_ACTION__NAME:
        return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT.equals(name);
      case SarlPackage.SARL_ACTION__PARAMETERS:
        return parameters != null && !parameters.isEmpty();
      case SarlPackage.SARL_ACTION__VARARGS:
        return varargs != VARARGS_EDEFAULT;
      case SarlPackage.SARL_ACTION__RETURN_TYPE:
        return returnType != null;
      case SarlPackage.SARL_ACTION__CREATE_EXTENSION_INFO:
        return createExtensionInfo != null;
      case SarlPackage.SARL_ACTION__EXCEPTIONS:
        return exceptions != null && !exceptions.isEmpty();
      case SarlPackage.SARL_ACTION__FIRED_EVENTS:
        return firedEvents != null && !firedEvents.isEmpty();
      case SarlPackage.SARL_ACTION__EXPRESSION:
        return expression != null;
    }
    return super.eIsSet(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public String toString()
  {
    if (eIsProxy()) return super.toString();

    StringBuffer result = new StringBuffer(super.toString());
    result.append(" (name: ");
    result.append(name);
    result.append(", varargs: ");
    result.append(varargs);
    result.append(')');
    return result.toString();
  }

} //SarlActionImpl
