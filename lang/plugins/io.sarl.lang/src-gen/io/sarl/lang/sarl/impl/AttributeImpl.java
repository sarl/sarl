/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.SarlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.xtext.common.types.JvmTypeReference;

import org.eclipse.xtext.xbase.XExpression;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Attribute</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.AttributeImpl#isWriteable <em>Writeable</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.AttributeImpl#getType <em>Type</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.AttributeImpl#getName <em>Name</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.AttributeImpl#getInitialValue <em>Initial Value</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class AttributeImpl extends EventFeatureImpl implements Attribute
{
  /**
   * The default value of the '{@link #isWriteable() <em>Writeable</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #isWriteable()
   * @generated
   * @ordered
   */
  protected static final boolean WRITEABLE_EDEFAULT = false;

  /**
   * The cached value of the '{@link #isWriteable() <em>Writeable</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #isWriteable()
   * @generated
   * @ordered
   */
  protected boolean writeable = WRITEABLE_EDEFAULT;

  /**
   * The cached value of the '{@link #getType() <em>Type</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getType()
   * @generated
   * @ordered
   */
  protected JvmTypeReference type;

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
   * The cached value of the '{@link #getInitialValue() <em>Initial Value</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getInitialValue()
   * @generated
   * @ordered
   */
  protected XExpression initialValue;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected AttributeImpl()
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
    return SarlPackage.Literals.ATTRIBUTE;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public boolean isWriteable()
  {
    return writeable;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setWriteable(boolean newWriteable)
  {
    boolean oldWriteable = writeable;
    writeable = newWriteable;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.ATTRIBUTE__WRITEABLE, oldWriteable, writeable));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public JvmTypeReference getType()
  {
    return type;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetType(JvmTypeReference newType, NotificationChain msgs)
  {
    JvmTypeReference oldType = type;
    type = newType;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SarlPackage.ATTRIBUTE__TYPE, oldType, newType);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setType(JvmTypeReference newType)
  {
    if (newType != type)
    {
      NotificationChain msgs = null;
      if (type != null)
        msgs = ((InternalEObject)type).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SarlPackage.ATTRIBUTE__TYPE, null, msgs);
      if (newType != null)
        msgs = ((InternalEObject)newType).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SarlPackage.ATTRIBUTE__TYPE, null, msgs);
      msgs = basicSetType(newType, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.ATTRIBUTE__TYPE, newType, newType));
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
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.ATTRIBUTE__NAME, oldName, name));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public XExpression getInitialValue()
  {
    return initialValue;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetInitialValue(XExpression newInitialValue, NotificationChain msgs)
  {
    XExpression oldInitialValue = initialValue;
    initialValue = newInitialValue;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SarlPackage.ATTRIBUTE__INITIAL_VALUE, oldInitialValue, newInitialValue);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setInitialValue(XExpression newInitialValue)
  {
    if (newInitialValue != initialValue)
    {
      NotificationChain msgs = null;
      if (initialValue != null)
        msgs = ((InternalEObject)initialValue).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SarlPackage.ATTRIBUTE__INITIAL_VALUE, null, msgs);
      if (newInitialValue != null)
        msgs = ((InternalEObject)newInitialValue).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SarlPackage.ATTRIBUTE__INITIAL_VALUE, null, msgs);
      msgs = basicSetInitialValue(newInitialValue, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.ATTRIBUTE__INITIAL_VALUE, newInitialValue, newInitialValue));
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
      case SarlPackage.ATTRIBUTE__TYPE:
        return basicSetType(null, msgs);
      case SarlPackage.ATTRIBUTE__INITIAL_VALUE:
        return basicSetInitialValue(null, msgs);
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
      case SarlPackage.ATTRIBUTE__WRITEABLE:
        return isWriteable();
      case SarlPackage.ATTRIBUTE__TYPE:
        return getType();
      case SarlPackage.ATTRIBUTE__NAME:
        return getName();
      case SarlPackage.ATTRIBUTE__INITIAL_VALUE:
        return getInitialValue();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SarlPackage.ATTRIBUTE__WRITEABLE:
        setWriteable((Boolean)newValue);
        return;
      case SarlPackage.ATTRIBUTE__TYPE:
        setType((JvmTypeReference)newValue);
        return;
      case SarlPackage.ATTRIBUTE__NAME:
        setName((String)newValue);
        return;
      case SarlPackage.ATTRIBUTE__INITIAL_VALUE:
        setInitialValue((XExpression)newValue);
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
      case SarlPackage.ATTRIBUTE__WRITEABLE:
        setWriteable(WRITEABLE_EDEFAULT);
        return;
      case SarlPackage.ATTRIBUTE__TYPE:
        setType((JvmTypeReference)null);
        return;
      case SarlPackage.ATTRIBUTE__NAME:
        setName(NAME_EDEFAULT);
        return;
      case SarlPackage.ATTRIBUTE__INITIAL_VALUE:
        setInitialValue((XExpression)null);
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
      case SarlPackage.ATTRIBUTE__WRITEABLE:
        return writeable != WRITEABLE_EDEFAULT;
      case SarlPackage.ATTRIBUTE__TYPE:
        return type != null;
      case SarlPackage.ATTRIBUTE__NAME:
        return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT.equals(name);
      case SarlPackage.ATTRIBUTE__INITIAL_VALUE:
        return initialValue != null;
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
    result.append(" (writeable: ");
    result.append(writeable);
    result.append(", name: ");
    result.append(name);
    result.append(')');
    return result.toString();
  }

} //AttributeImpl
