const express = require('express');
const app = express();
const port = process.env.PORT || 3000;

// Middleware to parse JSON bodies
app.use(express.json());

// Simple GET endpoint
app.get('/', (req, res) => {
  res.send('API Test Server is running!');
});

// Example GET endpoint
app.get('/api/items', (req, res) => {
  res.json([{ id: 1, name: 'Item 1' }, { id: 2, name: 'Item 2' }]);
});

// Example POST endpoint
app.post('/api/items', (req, res) => {
  const newItem = req.body;
  console.log('Received new item:', newItem);
  // In a real scenario, you might add validation and save to a database
  if (!newItem || !newItem.name) {
    return res.status(400).json({ error: 'Item name is required' });
  }
  res.status(201).json({ id: Date.now(), ...newItem }); // Respond with the created item (mocked ID)
});

// Example GET endpoint with parameters
app.get('/api/items/:id', (req, res) => {
    const itemId = parseInt(req.params.id, 10);
    // Mock finding an item
    if (itemId === 1) {
        res.json({ id: 1, name: 'Item 1 Detail' });
    } else if (itemId === 2) {
        res.json({ id: 2, name: 'Item 2 Detail' });
    } else {
        res.status(404).json({ error: 'Item not found' });
    }
});


app.listen(port, () => {
  console.log(`API Test Server listening at http://localhost:${port}`);
});

module.exports = app; // Export for potential testing
