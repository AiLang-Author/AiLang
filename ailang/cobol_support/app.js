// API Client
const API = {
    baseURL: '',  // Same origin
    
    async get(endpoint) {
        const response = await fetch(`/api${endpoint}`);
        return response.json();
    },
    
    async post(endpoint, data) {
        const response = await fetch(`/api${endpoint}`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(data)
        });
        return response.json();
    }
};

// State management (simple)
const State = {
    claims: [],
    beneficiaries: [],
    currentView: 'claims'
};

// UI Updates
function renderClaims() {
    const tbody = document.getElementById('claimsBody');
    tbody.innerHTML = State.claims.map(claim => `
        <tr onclick="viewClaim('${claim.claim_id}')">
            <td>${claim.claim_id}</td>
            <td>${claim.ssn}</td>
            <td>${claim.claim_type}</td>
            <td>$${claim.amount.toLocaleString()}</td>
            <td><span class="badge badge-${claim.status.toLowerCase()}">${claim.status}</span></td>
            <td>${claim.filed_date}</td>
            <td>
                <button onclick="processClaim('${claim.claim_id}'); event.stopPropagation();">Process</button>
            </td>
        </tr>
    `).join('');
}

function renderStats() {
    const total = State.claims.length;
    const pending = State.claims.filter(c => c.status === 'PENDING').length;
    const approved = State.claims.filter(c => c.status === 'APPROVED').length;
    const totalAmount = State.claims
        .filter(c => c.status === 'APPROVED')
        .reduce((sum, c) => sum + c.amount, 0);
    
    document.getElementById('totalClaims').textContent = total;
    document.getElementById('pendingClaims').textContent = pending;
    document.getElementById('approvedClaims').textContent = approved;
    document.getElementById('totalAmount').textContent = `$${totalAmount.toLocaleString()}`;
}

// Event handlers
function switchView(viewName) {
    // Hide all views
    document.querySelectorAll('.view').forEach(v => v.classList.remove('active'));
    document.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));
    
    // Show selected view
    document.getElementById(`${viewName}View`).classList.add('active');
    document.querySelector(`[data-view="${viewName}"]`).classList.add('active');
    
    State.currentView = viewName;
    
    // Load data for view
    if (viewName === 'claims') loadClaims();
    if (viewName === 'beneficiaries') loadBeneficiaries();
    if (viewName === 'reports') renderStats();
}

async function loadClaims() {
    try {
        State.claims = await API.get('/claims');
        renderClaims();
    } catch (error) {
        console.error('Failed to load claims:', error);
    }
}

async function processClaim(claimId) {
    try {
        const result = await API.post('/claims/process', { claim_id: claimId });
        alert(`Claim ${claimId}: ${result.status}`);
        loadClaims();
    } catch (error) {
        alert('Error processing claim');
    }
}

// Initialize
document.addEventListener('DOMContentLoaded', () => {
    // Tab navigation
    document.querySelectorAll('.tab').forEach(tab => {
        tab.addEventListener('click', (e) => {
            switchView(e.target.dataset.view);
        });
    });
    
    // Load initial data
    loadClaims();
});